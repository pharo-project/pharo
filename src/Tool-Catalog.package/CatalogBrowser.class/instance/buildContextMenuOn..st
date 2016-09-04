private - utilities
buildContextMenuOn: aList
	| col |
	col := (PragmaCollector new
		filter:
			[ :prg | prg keyword = #contextMenu and: [ prg methodClass = self class ] ])
		reset.
	"the pragma menu builder is probably doing a better job than this code but it was unclear"
	(col collected
		sorted: [ :p1 :p2 | p2 methodSelector > p1 methodSelector ])
		do: [ :each | self perform: each methodSelector with: aList ]