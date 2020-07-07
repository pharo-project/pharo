"
request reification of the class
"
Class {
	#name : #RFClassReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Reifications'
}

{ #category : #'plugin interface' }
RFClassReification class >> entities [
	^{RBProgramNode. Variable}
]

{ #category : #'plugin interface' }
RFClassReification class >> key [
	^#class
]

{ #category : #generate }
RFClassReification >> genForInstanceVariableSlot [
	^self generate
]

{ #category : #generate }
RFClassReification >> genForLiteralVariable [
	^self generate
]

{ #category : #generate }
RFClassReification >> genForRBProgramNode [
	^self generate
]

{ #category : #generate }
RFClassReification >> generate [
	^RBMessageNode 
		receiver: (RBVariableNode named: 'self')
		selector: #class
]
