Extension { #name : #TRBProgramNodeVisitor }

{ #category : #'*Reflectivity' }
TRBProgramNodeVisitor >> visitLiteralValueNode: aNode [
	"Redirect the message by default to #visitLiteralNode: for retrocompatibility (pharo 8)"

	^ self visitLiteralNode: aNode
]

{ #category : #'*Reflectivity' }
TRBProgramNodeVisitor >> visitLiteralVariableNode: aNode [
]

{ #category : #'*Reflectivity' }
TRBProgramNodeVisitor >> visitStoreIntoTempNode: aNode [
	^ aNode
]

{ #category : #'*Reflectivity' }
TRBProgramNodeVisitor >> visitStorePopIntoTempNode: aNode [
	^ aNode
]
