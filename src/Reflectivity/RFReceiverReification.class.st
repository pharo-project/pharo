"
I reify the receiver of a Message or a method
"
Class {
	#name : #RFReceiverReification,
	#superclass : #RFReification,
	#category : #'Reflectivity-Links'
}

{ #category : #'plugin interface' }
RFReceiverReification class >> entities [
	^{RBMethodNode. RBMessageNode}
]

{ #category : #'plugin interface' }
RFReceiverReification class >> key [
	^#receiver
]

{ #category : #generate }
RFReceiverReification >> genForRBMessageNode [
	^RBTemporaryNode named: #RFReceiverReificationVar.
]

{ #category : #generate }
RFReceiverReification >> genForRBMethodNode [
	^RBVariableNode named: 'self'
	
]

{ #category : #generate }
RFReceiverReification >> preamble: aNode [
	^aNode isMessage 
		ifTrue: [self preambleForMessage: aNode]
		ifFalse: [{}]
]

{ #category : #generate }
RFReceiverReification >> preambleForMessage: aNode [
	| preamble |
	preamble := OrderedCollection new.

	(1 to: aNode numArgs) reverseWithIndexDo: [:each :index |  
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			preamble add:  (RFStorePopIntoTempNode named: name).
	].
	preamble 
		add: (RFStoreIntoTempNode named: #RFReceiverReificationVar).
		(1 to: aNode numArgs) withIndexDo: [:each :index |  
			| name |
			name := 'RFArg', index asString, 'RFReification'.
			preamble add:  (RBTemporaryNode named: name).
	].

	^ preamble
]
