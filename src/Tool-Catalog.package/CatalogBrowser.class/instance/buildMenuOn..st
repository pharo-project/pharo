private - utilities
buildMenuOn: aList
	| col |
	col := (PragmaCollector filter: [ :prg | prg keyword = #menuOrder: and: [ prg methodClass = self class ] ]) reset.
	col collected sort: [ :a :b | (a argumentAt: 1) <= (b argumentAt: 1) ].
	col do: [ :each | self perform: each methodSelector with: aList ]