Extension { #name : #Object }

{ #category : #'*NewMethodFinder' }
Object class >> approvedSelectors [

	^ #(at: basicAt: bsicSize yourself size
	    ->
	    = == ~= hash literalEqual
	   )
]
