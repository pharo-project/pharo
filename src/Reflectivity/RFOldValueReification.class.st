"
the orginal value of an assignement. Useful in an #after link

NOTE: Not yet working. Needs to gather information in a pre-hook
"
Class {
	#name : #RFOldValueReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFOldValueReification class >> entities [
	^{RBVariableNode. RBAssignmentNode}
]

{ #category : #'plugin interface' }
RFOldValueReification class >> key [
	^#oldValue
]

{ #category : #generate }
RFOldValueReification >> genForRBAssignmentNode [
	| varNode |
	varNode := entity variable.
	
	varNode isGlobal ifTrue: [^RFLiteralVariableNode value: entity binding value]. 
	^RBVariableNode named: varNode name.
]

{ #category : #generate }
RFOldValueReification >> genForRBVariableNode [
	"same as #value for variableNodes"
	entity isGlobal ifTrue: [^RFLiteralVariableNode value: entity binding value]. 
	^RBVariableNode named: entity name.
]

{ #category : #generate }
RFOldValueReification >> preamble: aNode [
		^{}
]
