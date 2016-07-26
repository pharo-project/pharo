expression types
temporariesIn: methodSelector
	" [ '|' (variable)* '|' ]"
	| vars theActualText |
	(self match: #verticalBar) ifFalse: 
		["no temps"
		doitFlag ifTrue: [self interactive
				ifFalse: [tempsMark := 1]
				ifTrue: [tempsMark := requestor selectionInterval first].
			^ #()].
		tempsMark := (prevEnd ifNil: [0]) + 1.
		tempsMark := hereMark	"formerly --> prevMark + prevToken".

		tempsMark > 0 ifTrue:
			[theActualText := source contents.
			[tempsMark < theActualText size and: [(theActualText at: tempsMark) isSeparator]]
				whileTrue: [tempsMark := tempsMark + 1]].
			^ #()].
	vars := OrderedCollection new.
	[hereType == #word]
		whileTrue: [vars addLast: (encoder bindTemp: self advance in: methodSelector)].
	(self match: #verticalBar) ifTrue: 
		[tempsMark := prevMark.
		^ vars].
	^ self expected: 'Vertical bar'