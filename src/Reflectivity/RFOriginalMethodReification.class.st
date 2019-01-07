"
I reify the method where the link is installed.

Note: this is the original method, that is, the one that has this link *not* installed.
"
Class {
	#name : #RFOriginalMethodReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFOriginalMethodReification class >> entities [
	^{RBProgramNode}.
]

{ #category : #'plugin interface' }
RFOriginalMethodReification class >> key [
	^#originalmethod
]

{ #category : #generate }
RFOriginalMethodReification >> genForRBProgramNode [
	^RFLiteralVariableNode value: entity methodNode method
]
