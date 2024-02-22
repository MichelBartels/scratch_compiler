/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <float> NUMBER
%token <string> STRING
%token LSQUAREBRACKET RSQUAREBRACKET
%token LCURLYBRACKET RCURLYBRACKET
%token COMMA
%token COLON
%token TRUE
%token FALSE
%token NULL
%token EOF


%start main
%type <Past.value> expr 
%type <(string * Past.value) list> exprobject
%type <Past.value list> exprarray
%type <Past.value> main
%%
main:
	expr EOF                { $1 }
;

expr:
| LCURLYBRACKET exprobject RCURLYBRACKET { Past.Object(get_loc (), $2) }
| LSQUAREBRACKET exprarray RSQUAREBRACKET { Past.Array(get_loc (), $2) }
| STRING { Past.String(get_loc (), $1) }
| NUMBER { Past.Number(get_loc (), $1) }
| TRUE { Past.Boolean(get_loc (), true) }
| FALSE { Past.Boolean(get_loc (), false) }
| NULL { Past.Null(get_loc ()) }

exprobject:
| { [] }
| STRING COLON expr { [($1, $3)] }
| STRING COLON expr COMMA exprobject { ($1, $3) :: $5 }

exprarray:
| { [] }
| expr { [$1] }
| expr COMMA exprarray { $1 :: $3 }
