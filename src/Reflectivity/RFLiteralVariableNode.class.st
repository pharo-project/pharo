"
I am used to reference either the MetaObject or the MetaLink (if inlining is turned off).

We are using a special kind of LiteralVariable instead of just plain literals so that we do not use non-standard literals. 
"
Class {
	#name : #RFLiteralVariableNode,
	#superclass : #RBProgramNode,
	#instVars : [
		'binding'
	],
	#category : #'Reflectivity-Compiler'
}

{ #category : #'instance creation' }
RFLiteralVariableNode class >> value: anObject [
	^self new binding: (AdditionalBinding key: #RFMetaBinding value: anObject)
]

{ #category : #visiting }
RFLiteralVariableNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitLiteralVariableNode:  self
]

{ #category : #accessing }
RFLiteralVariableNode >> binding [ 
	^binding
]

{ #category : #accessing }
RFLiteralVariableNode >> binding: anAssocation [
	binding := anAssocation
]

{ #category : #generation }
RFLiteralVariableNode >> dumpOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: ' value: '.
	self value printOn: aStream
]

{ #category : #accessing }
RFLiteralVariableNode >> start [
	^0
]

{ #category : #accessing }
RFLiteralVariableNode >> stop [
	^1
]
