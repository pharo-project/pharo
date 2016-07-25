printing/formatting
printStackExtraColumnForContext: aContext 

	| selector |
	^ aContext outerContext 
		ifNil: [ 
			String streamContents: [ :aStream |
				selector := aContext selector ifNil: [ aContext method defaultSelector ].
				selector = #doesNotUnderstand:
					ifTrue:[ 
						aStream space.
						(aContext tempAt: 1) selector printOn: aStream ]
					ifFalse: [ '' ] ] ]
		ifNotNil: [ aContext closure printString  ]