*Morphic-Base
borderWidth: aNumber
	| style |
	style := self borderStyle.
	style width = aNumber ifTrue: [ ^self ].

	style style = #none
		ifTrue: [ self borderStyle: (SimpleBorder width: aNumber color: Color transparent) ]
		ifFalse: [ style width: aNumber. self changed ].
