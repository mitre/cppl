/* A parser for CPPL programs.
   Copyright (C) 2005 The MITRE Corporation

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

%{

open Ast

%}

%token EOF
%token <Ast.pos * string> STRING CONST VARIABLE ID
%token <Ast.pos> ACCEPT AND ASSUMES CALL CASES CHANNEL CONNECT
%token <Ast.pos> END HASH IN LET MATCH MSG NAME NEW NONCE PROVES
%token <Ast.pos> PUBKEY RECEIVE RETURN SEND SYMKEY TEXT
%token <Ast.pos> ARROW BAR CANGLE CBRAC CBRACBAR CCURL CCURLBAR CPAREN
%token <Ast.pos> COLON COMMA EQUAL IMPLY OANGLE OBRAC OBRACBAR OCURL
%token <Ast.pos> OPAREN OCURLBAR PERIOD UNDERSCORE
%right COMMA

%start program

%type <Ast.program> program

%start axioms

%type <Ast.clause list> axioms

%%

program:
  procedures EOF                 { List.rev $1 }
;

axioms:
  clauses EOF                    { List.rev $1 }
;

clauses:
				 { [] }
| clauses literal body PERIOD    { ($2, $3) :: $1 }
;

procedures:
  procedure                      { [$1] }
| procedures procedure           { $2 :: $1 }
;

procedure:
  ID decls decls assumes proves statement END
  { $1, [], $2, $4, $5, $3, $6 }
;

decls:
  OPAREN CPAREN                  { [] }
| OPAREN decl_list CPAREN        { List.rev $2 }
;

decl_list:
  decl                           { [$1] }
| decl_list COMMA decl           { $3 :: $1 }
;

decl:
  ID                             { $1, None }
| ID COLON var_type              { $1, Some $3 }
;

key_type:
  PUBKEY                         { Pubkey $1 }
| SYMKEY                         { Symkey $1 }
;

var_type:
  NAME                           { Name $1 }
| TEXT                           { Text $1 }
| MSG                            { Msg $1 }
| NONCE                          { Nonce $1 }
| CHANNEL                        { Chan $1 }
| key_type                       { Key $1 }
;

assumes:
				 { [] }
| ASSUMES rely                   { $2 }
;

proves:
				 { [] }
| PROVES guar                    { $2 }
;

guar:
  UNDERSCORE                     { [] }
| literal_conjunct               { List.rev $1 }
;

rely:
  UNDERSCORE                     { [] }
| clause_conjunct                { List.rev $1 }
;

clause_conjunct:
  clause                         { [$1] }
| clause_conjunct AND clause     { $3 :: $1 }
;

clause:
  literal body	                 { $1, $2 }
;

literal_conjunct:
  literal                        { [$1] }
| literal_conjunct AND literal   { $3 :: $1 }
;

literal:
  ID                             { $1, [] }
| ID OPAREN terms CPAREN         { $1, (List.rev $3) }
| term EQUAL term                { ($2, "="), [$1; $3] }
;

term:
  decl                           { Identifier $1 }
| VARIABLE                       { Variable $1 }
| CONST                          { Constant $1 }
| STRING                         { Constant $1 }
;

terms:
  term                           { [$1] }
| terms COMMA term               { $3 :: $1 }
;

body:
				 { [] }
| IMPLY literals                 { List.rev $2 }
;

literals:
  literal                        { [$1] }
| literals COMMA literal         { $3 :: $1}
;

statement:
  RETURN                         { Return ($1, [], [], []) }
| LET decl EQUAL expression IN statement {
				   let id, t = $2 in
				   Let ($1, (id, ref t), $2, $4, $6) }
| SEND send                      { To ($1, [$2]) }
| SEND CASES sends END           { To ($1, List.rev $3) }
| SEND CASES END                 { To ($1, []) }
| MATCH ID receive               { From ($1, Match $2, [$3]) }
| MATCH ID CASES receives END    { From ($1, Match $2, List.rev $4) }
| MATCH ID CASES END             { From ($1, Match $2, []) }
| RECEIVE ID receive             { From ($1, Recv $2, [$3]) }
| RECEIVE ID CASES receives END  { From ($1, Recv $2, List.rev $4) }
| RECEIVE ID CASES END           { From ($1, Recv $2, []) }
| CALL invocation                { Call ($1, [$2]) }
| CALL CASES invocations END     { Call ($1, List.rev $3) }
| CALL CASES END                 { Call ($1, []) }
;

send:
  guar ARROW ID msg statement    { $2, [], $1, $3, $4, $5 }
;

sends:
  send                           { [$1] }
| sends BAR send                 { $3 :: $1 }
;

receive:
  msg ARROW rely statement       { $2, [], $1, $3, $4 }
;

receives:
  receive                        { [$1] }
| receives BAR receive           { $3 :: $1 }
;

invocation:
  guar ARROW call statement      { $2, [], $1, $3, $4 }
;

call:
  UNDERSCORE                     { None }
| name decls decls rely          { Some ($1, $2, [], $3, $4) }
;

name:
  path ID                        { List.rev $1, $2 }
;

path:
				 { [] }
| path VARIABLE PERIOD           { $2 :: $1 }
;

invocations:
  invocation                     { [$1] }
| invocations BAR invocation     { $3 :: $1}
;

msg:
  decl                           { Decl $1 }
| msg COMMA msg                  { Cat ($2, $1, $3) }
| OCURL msg CCURL decl           { Encrypt ($1, $2, $4, false) }
| OCURLBAR msg CCURLBAR decl     { Encrypt ($1, $2, $4, true) }
| OBRAC msg CBRAC decl           { Sign ($1, $2, $4, false) }
| OBRACBAR msg CBRACBAR decl     { Sign ($1, $2, $4, true) }
| HASH OPAREN msg CPAREN         { Hash ($1, $3) }
| OANGLE ID EQUAL msg CANGLE     { Bind ($2, $4) }
| CONST COMMA msg                { let p, t = $1 in Tag(p, t, $3) }
| VARIABLE COMMA msg             { let p, t = $1 in Tag(p, t, $3) }
| OPAREN msg CPAREN              { $2 }
;

expression:
  ID                             { Ide $1 }
| NEW key_type                   { New_key $2 }
| NEW NONCE                      { New_nonce $1 }
| CONNECT OPAREN ID CPAREN       { Connect $3 }
| ACCEPT                         { Accept $1 }
| RECEIVE OPAREN ID CPAREN       { Receive $3 }
;
