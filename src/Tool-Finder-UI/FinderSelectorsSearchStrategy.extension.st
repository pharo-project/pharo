Extension { #name : 'FinderSelectorsSearchStrategy' }

{ #category : '*Tool-Finder-UI' }
FinderSelectorsSearchStrategy >> rootNodeClassForResult: aCollection [

	^ aCollection size > 1
		  ifTrue: [ FinderMethodNode ]
		  ifFalse: [ FinderSingleMethodNode ]
]
