"
reify newValue
"
Class {
	#name : #RFNewValueReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFNewValueReification class >> entities [
	^{RBVariableNode. RBAssignmentNode}
]

{ #category : #'plugin interface' }
RFNewValueReification class >> key [
	^#newValue
]

{ #category : #generate }
RFNewValueReification >> genForRBAssignmentNode [
	^RBTemporaryNode named: #RFNewValueReificationVar.
]

{ #category : #generate }
RFNewValueReification >> genForRBVariableNode [
	"same as #value for variableNodes"
	^RBTemporaryNode named: #RFNewValueReificationVar.
]

{ #category : #preamble }
RFNewValueReification >> preambleForAssignment: aNode [
	"balance stack for instead"
	link control= #instead
		ifTrue: [ ^RFStorePopIntoTempNode named: #RFNewValueReificationVar  ]
		ifFalse: [^RFStoreIntoTempNode named: #RFNewValueReificationVar  ]
	
	
]

{ #category : #preamble }
RFNewValueReification >> preambleForVariable: aNode [
	"balance stack for instead"
	link control= #instead
		ifTrue: [ ^RFStorePopIntoTempNode named: #RFNewValueReificationVar  ]
		ifFalse: [^RFStoreIntoTempNode named: #RFNewValueReificationVar  ]
	
	
]
