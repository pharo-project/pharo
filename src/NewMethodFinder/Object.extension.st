Extension { #name : #Object }

{ #category : #'*NewMethodFinder' }
Object class >> approvedSelectors [

	^ #(at: basicAt: basicSize yourself size
	    ->
	    = == ~= hash literalEqual
	   )
]
