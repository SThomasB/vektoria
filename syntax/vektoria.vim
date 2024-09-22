if exists("b:current_syntax")
  finish
endif

" Define keywords
syn keyword vektoriaKeyword let in

" Define conditional operators
syn match vektoriaConditional "?"

" Define function-like calls (starting with @)
syn match vektoriaFunction "@[a-zA-Z_]\w*"

" Define comparison and logic operators
syn match vektoriaOperator "->\|==\|<\|>\|+"

" Define numbers (constants)
syn match vektoriaNumber /\v\d+/

" Define strings (quoted)
syn region vektoriaString start=/"/ end=/"/

" Define comments (single-line)
syn match vektoriaComment "--.*"

" Highlight groups
hi def link vektoriaKeyword Keyword
hi def link vektoriaConditional Conditional
hi def link vektoriaFunction Function
hi def link vektoriaOperator Operator
hi def link vektoriaNumber Number
hi def link vektoriaString String
hi def link vektoriaComment Comment

let b:current_syntax = "vektoria"
