examples - cascades
exampleCascadeIntoBlockWithTempIfTrueIfFalse
	| t |
	t := OrderedCollection new.
	t yourself
		add: {1};
		add: {2};
		add: {3};
		collect: [ :anArray | 
					anArray
						collect: [ :aNumber | 
									aNumber isNumber
										ifFalse: [ anArray first ] ];
						collect: [ :aNumber | 
									aNumber isNumber
										ifTrue: [ 
											aNumber even
												ifTrue: [ #('even') ]
												ifFalse: [ #('odd') ] ]
										ifFalse: [ 0 ] ] ]