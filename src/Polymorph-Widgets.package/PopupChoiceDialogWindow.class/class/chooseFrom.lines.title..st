instance creation
chooseFrom: aList lines: lines title: title
	"self chooseFrom: #('yes' 'no') lines: #(1 2)  title: 'Foo is the question'"
	"self chooseFrom: #('yes' 'no') lines: #()  title: 'Foo is the question'"
	| pd |
	pd := (self newWithTheme: World theme)
		title: (title isEmpty ifTrue: ['Choose' translated] ifFalse: [title asString]);
		labels: aList;
		lines: (lines ifNil: [#()]);
		model: aList.
	"World may be not the best choice because a window may want to be in controlaList 
	this point should be investigated based on UIManager default modalMorph"
	^(World openModal: pd) choice