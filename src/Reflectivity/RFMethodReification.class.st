"
I reify the method where the link is installed.

Note: this is the *current* method, that is, the one that has the code of the link.

it is the same that you would get with ""thisContext method"", without the need to access the context (which is slow)
"
Class {
	#name : #RFMethodReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFMethodReification class >> entities [
	^{RBProgramNode}.
]

{ #category : #'plugin interface' }
RFMethodReification class >> key [
	^#method
]

{ #category : #generate }
RFMethodReification >> genForRBProgramNode [
	^RBMessageNode 
		receiver: (RBMessageNode receiver: (RFLiteralVariableNode value: entity)  selector: #methodNode)
		selector: #method
]
