"
reify newValue
"
Class {
	#name : #RFNewValueReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
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
	^RBTemporaryNode named: #RFNewValueReificationVar.
]

{ #category : #generate }
RFNewValueReification >> preamble: aNode [
		link control= #instead 
		ifTrue: [ ^RFStorePopIntoTempNode named: #RFNewValueReificationVar  ]
		ifFalse: [^RFStoreIntoTempNode named: #RFNewValueReificationVar  ]
	
	
]
