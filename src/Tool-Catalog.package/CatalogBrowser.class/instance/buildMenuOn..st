private - utilities
buildMenuOn: aList
	| col |
	col := (PragmaCollector new
		filter: [ :prg | prg keyword = #menu and: [ prg methodClass = self class ] ])
		reset.
	col do: [ :each | self perform: each methodSelector with: aList ]