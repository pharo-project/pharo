"
I reify operations like message send, variable read, assignment....
"
Class {
	#name : #RFOperationReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFOperationReification class >> entities [
	^{RBVariableNode}
]

{ #category : #'plugin interface' }
RFOperationReification class >> key [
	^#operation
]

{ #category : #generate }
RFOperationReification >> genForRBVariableNode [
	entity isInstance ifTrue: [  
		^RBParser parseExpression: ('RFSlotRead new 
			object: self;
			variableName: #{1}.' format: {entity name})].
		
	entity isTemp ifTrue: [
		^RBParser parseExpression: ('RFTempRead new 
			context: thisContext;
			variableName: #{1}.' format: {entity name})].
		
	self error: 'variable type not yet supported'



]
