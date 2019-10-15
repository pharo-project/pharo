Extension { #name : #RBProgramNode }

{ #category : #'*NECompletion' }
RBProgramNode >> completionToken [
	^ String empty
]

{ #category : #'*NECompletion' }
RBProgramNode >> typeForCompletionIfAbsent: aBlock [

 	^ self propertyAt: #type ifAbsent: [ ^ aBlock value ]
]
