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
	^{OCVariableNode. OCAssignmentNode}
]

{ #category : 'plugin interface' }
RFNewValueReification class >> key [
	^#newValue
]

{ #category : 'generate' }
RFNewValueReification >> genForASTAssignmentNode [
	^OCVariableNode named: #RFNewValueReificationVar
]

{ #category : 'generate' }
RFNewValueReification >> genForASTVariableNode [
	"same as #value for variableNodes"
	^OCVariableNode named: #RFNewValueReificationVar
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
