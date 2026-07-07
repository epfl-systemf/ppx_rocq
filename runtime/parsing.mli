(** Methods for parsing terms, tactics, etc. *)

open Terms

(** {1 Parsing functions} *)

val parse : ?loc:Loc.t -> 'a Procq.Entry.t -> string -> 'a
(** [parse entry s] parses string [s] using the grammar rule associated to the
    [entry].

    See {!module:Procq.Constr} for a list of pre-defined entries.
 *)

val parse_constrexpr : ?loc:Loc.t -> string -> constrexpr
(** [parse_constrexpr s] parses the AST of a Rocq term from string [s].

    {!parse_constrexpr} is the most basic method for parsing terms: it does not
    resolve names or implicit arguments, nor does it not type-check the term
    obtained by the parser. To obtain terms with more guarantees, use
    {!glob_constr_of_string} or {!constr_of_string} instead.
  *)

val parse_ident : ?loc:Loc.t -> string -> Names.Id.t
(** [parse_ident s] parses an identifier from string [s]. *)

val parse_qualid : ?loc:Loc.t -> string -> Libnames.qualid
(** [parse_qualid s] parses a qualified identifier from string [s]. *)

val parse_vernac : ?loc:Loc.t -> string -> Vernacexpr.vernac_control
(** [parse_vernac s] parse the vernacular command [s].

    The command [s] can include meta-vernaculars such as [Time] or [Fail]. *)

val parse_ltac : ?loc:Loc.t -> string -> Ltac_plugin.Tacexpr.raw_tactic_expr
(** [parse_ltac s] parses an Ltac1 expression from string [s]. *)

val parse_ltac2 : ?loc:Loc.t -> string -> Ltac2_plugin.Tac2expr.raw_tacexpr
(** [parse_ltac2 s] parses an Ltac2 expression from string [s]. *)

val parse_match_pattern : ?loc:Loc.t -> string -> constrexpr
(** [parse_match_pattern s] parse an Ltac/Ltac2 match pattern from string [s]. *)

(** {1 Parsing tactics} *)

val glob_constr_of_string : ?loc:Loc.t -> string -> glob_constr Proofview.tactic
(** [glob_constr_of_string s] parses a Rocq term from string [s], globalizing
    names and resolving notations.

    The resulting term is not type-checked. To type-check it, use
    {!Terms.Constr.of_glob_constr} instead.
 *)

val constr_of_string : ?loc:Loc.t -> string -> constr Proofview.tactic
(** [constr_of_string s] parses an evar-free Rocq term from string [s]. *)

val open_constr_of_string : ?loc:Loc.t -> string -> open_constr Proofview.tactic
(** [open_constr_of_string s] behaves like {!constr_of_string}, but evars are
    allowed in the resulting term. *)

val match_pattern_of_string : ?loc:Loc.t -> string -> pattern Proofview.tactic
(** [match_pattern_of_string s] parses and interprets pattern [s]. *)

(** {1 Parsing with antiquotations} *)

(** An antiquotation is a part of a term that is substituted by an OCaml
    expression. Antiquotations are denoted by [%{x}] or [%kind:{x}], where [x]
    is a valid OCaml expression. Methods that can handle antiquotations are
    called {i quasi-parsing methods}.

    For example, while ["1 + 1"] can be immediately parsed to a term, parsing
    and interpreting ["1 + %{x}"] requires substituting the OCaml value [x]
    before continuing.

    The implementation of quasi-parsing proceeds as follows:

    1. Antiquoted values are replaced with holes: the expression ["%{x} + %{y}"]
       is parsed as ["□₀ + □₁"], where [□] is a special placeholder. For
       simplicity, we assign a natural number to each hole in order, so that
       substitution is a simple array lookup.

    2. The expression is converted up to the target term representation, via
       internalization ([glob_constr]) or interpretation ([constr]).

    3. The obtained expression is memoized, so that the common part of each quasiterm
       is shared between different substitutions.

    4. Given a context (i.e. array of expressions), we perform the substitution
       according to the following table (“source” is the type of antiquotation,
       while “target” refers to the final desired term representation):

       {t
         | Source \ Target | constrexpr  | glob_constr | (open_)constr |
         | :-------------: | :---------: | :---------: | :-----------: |
         | constrexpr      | identity    | internalize | interp        |
         | glob_constr     | genarg      | identity    | pretype       |
         | (open_)constr   | genarg      | genarg      | identity      |
       }
 *)

(** Types of antiquotations. *)
type antiquotation =
  [ `Constr of constr           (** [%{…}] or [%constr:{…}] *)
  | `Open_constr of open_constr (** [%open_constr:{…}] *)
  | `Preterm of glob_constr     (** [%preterm:{…}] *)
  | `Expr of constrexpr         (** [%expr:{…}] *)
  ]

val quasiparse_constrexpr : ?loc:Loc.t -> string -> (antiquotation array -> constrexpr)
(** [quasiparse_constrexpr s context] behaves like [parse_constexpr s], except that
    antiquotations of the form [%{n}] are replaced by [context.(n)]. *)

val glob_constr_of_quasistring : ?loc: Loc.t -> string -> (antiquotation array -> glob_constr) Proofview.tactic
(** [let* f = glob_constr_of_quasistring s in f context] behaves like [glob_constr_of_string s],
    except that antiquotations of the form [%{n}] are replaced by [context.(n)]. *)

val constr_of_quasistring : ?loc:Loc.t -> string -> (antiquotation array -> constr Proofview.tactic) Proofview.tactic
(** [let* f = constr_of_quasistring s in f context] behaves like [constr_of_string s], except that
    antiquotations of the form [%{n}] are replaced by [context.(n)]. *)

val open_constr_of_quasistring : ?loc:Loc.t -> string -> (antiquotation array -> open_constr Proofview.tactic) Proofview.tactic
(** [let* f = open_constr_of_quasistring s in f context] behaves like
    [let* f = constr_of_string s in f context], except that antiquotations of
    the form [%{n}] are replaced by [context.(n)]. *)

val substitute : (antiquotation array -> 'a Proofview.tactic) Proofview.tactic -> antiquotation array -> 'a Proofview.tactic
(** [substitute term_with_holes values] is equivalent to [let* t = term_with_holes in t values]. *)
