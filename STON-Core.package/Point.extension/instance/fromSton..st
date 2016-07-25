*ston-core
fromSton: stonReader
	stonReader parseListDo: [ :each :index |
		index = 1 ifTrue: [ x := each ].
		index = 2 ifTrue: [ y := each ] ]