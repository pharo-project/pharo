Extension { #name : #RBMessageNode }

{ #category : #'*NECompletion' }
RBMessageNode >> completionToken [

	^ self selector
]

{ #category : #'*NECompletion' }
RBMessageNode >> narrowWith: aString [

 	self selector: aString asSymbol
]

{ #category : #'*NECompletion' }
RBMessageNode >> typeForCompletionIfAbsent: aBlock [

 	^ self receiver typeForCompletionIfAbsent: aBlock
]
