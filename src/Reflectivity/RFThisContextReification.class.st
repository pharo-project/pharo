"
I can be used to pass the context to the meta object
"
Class {
	#name : #RFThisContextReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFThisContextReification class >> entities [
	^{RBProgramNode . LiteralVariable . Slot}
]

{ #category : #'plugin interface' }
RFThisContextReification class >> key [
	^#context
]

{ #category : #generate }
RFThisContextReification >> genForInstanceVariableSlot [
	^RBThisContextNode named: #thisContext
]

{ #category : #generate }
RFThisContextReification >> genForLiteralVariable [
	^RBThisContextNode named: #thisContext
]

{ #category : #generate }
RFThisContextReification >> genForRBMethodNode [
	"when in after the call to the meta is wrapped in a block"
	^link control = #after
		ifTrue: [ RBMessageNode receiver: (RBThisContextNode named: #thisContext) selector: #outerContext]
		ifFalse: [ RBThisContextNode named: #thisContext ]

]

{ #category : #generate }
RFThisContextReification >> genForRBProgramNode [
	^RBThisContextNode named: #thisContext
]
