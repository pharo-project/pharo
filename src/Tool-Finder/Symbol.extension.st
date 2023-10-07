Extension { #name : 'Symbol' }

{ #category : '*Tool-Finder' }
Symbol class >> forbiddenSelectorsForMethodFinder [

	^ super forbiddenSelectorsForMethodFinder , #( string: #privateAt:put: )
]
