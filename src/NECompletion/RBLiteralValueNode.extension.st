Extension { #name : #RBLiteralValueNode }

{ #category : #'*NECompletion' }
RBLiteralValueNode >> completionToken [
	^ self value asString
]
