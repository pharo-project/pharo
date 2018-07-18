Extension { #name : #Boolean }

{ #category : #'*NewMethodFinder' }
Boolean class >> approvedSelectors [
	^ #(#not #asBit #and: #or: & | #xor: #ifFalse: #ifTrue #ifFalse:ifTrue #ifTrue:ifFalse)
]
