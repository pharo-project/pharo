examples - branches
exampleIfTrueIfFalseNested3
	^ {1.2.3.4}
		collect: [ :aNumber | 
			aNumber isNumber
				ifTrue: [ 
					aNumber even
						ifTrue: [ #('even') ]
						ifFalse: [ #('odd') ] ]
				ifFalse: [ 0 ] ]