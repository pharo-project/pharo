"
reify newValue
"
Class {
	#name : 'RFNewValueReification',
	#superclass : 'RFReification',
	#category : 'Reflectivity-Reifications',
	#package : 'Reflectivity',
	#tag : 'Reifications'
}

{ #category : 'plugin interface' }
RFNewValueReification class >> entities [
	^{ASTVariableNode. ASTAssignmentNode}
]

{ #category : 'plugin interface' }
RFNewValueReification class >> key [
	^#newValue
]

{ #category : 'generate' }
RFNewValueReification >> genForRBAssignmentNode [
	^ASTVariableNode named: #RFNewValueReificationVar
]

{ #category : 'generate' }
RFNewValueReification >> genForRBVariableNode [
	"same as #value for variableNodes"
	^ASTVariableNode named: #RFNewValueReificationVar
]

{ #category : 'preamble' }
RFNewValueReification >> preambleForAssignment: aNode [
	"balance stack for instead"

	^ link control = #instead
		ifTrue: [ RFStorePopIntoTempNode named: #RFNewValueReificationVar ]
		ifFalse: [ RFStoreIntoTempNode named: #RFNewValueReificationVar ]
]

{ #category : 'preamble' }
RFNewValueReification >> preambleForVariable: aNode [
	"balance stack for instead"

	^ link control = #instead
		ifTrue: [ RFStorePopIntoTempNode named: #RFNewValueReificationVar ]
		ifFalse: [ RFStoreIntoTempNode named: #RFNewValueReificationVar ]
]
