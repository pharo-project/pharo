Extension { #name : #Symbol }

{ #category : #'*Tool-Finder' }
Symbol class >> forbiddenSelectorsForMethodFinder [

	^ #(string: privateAt:put:)
]
