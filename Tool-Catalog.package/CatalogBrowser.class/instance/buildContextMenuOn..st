private - utilities
buildContextMenuOn: aList
	| col |
	col := (PragmaCollector new
		filter: [ :prg | prg keyword = #contextMenu and: [ prg methodClass = self class ] ]) reset.
	"the pragma menu builder is probably doing a better job than this code but it was unclear"
	(col collected sorted: [:p1 :p2 | p2 selector > p1 selector]) do: [ :each | self perform: each selector with: aList ]