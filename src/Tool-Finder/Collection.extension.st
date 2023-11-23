Extension { #name : 'Collection' }

{ #category : '*Tool-Finder' }
Collection class >> approvedSelectorsForMethodFinder [

	 ^ self selectors
]

{ #category : '*Tool-Finder' }
Collection class >> forbiddenSelectorsForMethodFinder [

	^ #( #combinations )
]
