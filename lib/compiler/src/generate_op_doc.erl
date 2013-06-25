-module(generate_op_doc).

-export([docbook/2]).

-record(op, {name="", opcode, doc="", spec="", deprecated=false, prev=[]}).

docbook(InFile, OutFile) ->
    {ok, File} = file:open(InFile, [read]),
    Op = parse(File),
    docbook_format(Op).

docbook_format(Op) ->
    docbook_format_line(Op)
        ++
        "|-------------------------------------------------\n".

docbook_format_line(#op{name=Name,
                        opcode=Opcode,
                        doc=Doc,
                        spec=Spec,
                        deprecated=Deprecated,
                        prev=Prev}) ->
    docbook_format_line(Prev) ++
        "|" ++ strip(Name) ++
        "|" ++  format_opcode(Opcode, Deprecated) ++
        "|" ++  format_spec(Spec, Deprecated) ++
        "|" ++  strip(Doc) ++ "\n";
docbook_format_line([]) ->
    "|-------------------------------------------------\n" ++
        "| Name | Op Code | Spec | Documentation\n".


format_spec([Name|Args], false) ->
    "*"++strip(Name)++"*" ++ " "
        ++ string:join([format_arg(A) || A<-Args], ", ");
format_spec(_, true) -> "*DEPRECATED*";
format_spec([], _) -> "".

format_arg(A) -> "_"++strip(A)++"_".

format_opcode(undefined, _Deprecated) ->
    "";
format_opcode(Opcode, Deprecated) ->
    if
        Deprecated -> "(";
        true -> ""
    end ++
        strip(Opcode) ++
        if
            Deprecated -> ")";
            true -> ""
        end.



strip(S) ->
 [ esacpe(Char)
  || Char <- string:strip(S, right, 10)].

esacpe(Char) ->
    case Char of
        $| -> "\|";
        $\n -> " ";
        _ -> Char
    end.

parse(File) ->
    parse(File, #op{}).

parse(File, Op) ->
    case file:read_line(File) of
        {ok, Line} ->
            parse(File, parse_line(Line, Op));
        eof -> Op
    end.

parse_line("##" ++ Rest, Op) ->
    parse_doc(Rest, Op);
parse_line("#" ++ _, Op) -> Op;
parse_line([N|_]=Line, Op) when N >= $0, N =< $9 ->
    [OpNo, MFA] = string:tokens(Line, ": "),
    case MFA of
        "-" ++ Name ->
            Op#op{name=Name, opcode=OpNo, deprecated=true, prev=Op};
        _ ->
            #op{name=MFA, opcode=OpNo, prev=Op}
    end;
parse_line(_, Op) ->
    Op.

parse_doc(Line, Op) ->
    case string:tokens(Line, " ") of
        ["@spec" | Rest] -> Op#op{spec=Rest};
        ["@doc" | Rest] -> Op#op{doc=string:join(Rest, " ")};
        _ -> Op#op{doc=Op#op.doc++Line}
    end.
