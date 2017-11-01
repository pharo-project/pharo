"
I reify the receiver of a Message or a method
"
Class {
	#name : #RFEReceiverReification,
	#superclass : #RFReification,
	#category : #Reflectivity-Links
}

{ #category : #'plugin interface' }
RFEReceiverReification class >> entities [
	^{RBMethodNode. RBMessageNode}
]

{ #category : #'plugin interface' }
RFEReceiverReification class >> key [
	^#receiver
]

{ #category : #generate }
RFEReceiverReification >> genForRBMessageNode [
	^RBTemporaryNode named: #RFReceiverReificationVar.
]

{ #category : #generate }
RFEReceiverReification >> genForRBMethodNode [
	^RBMessageNode 
		receiver: (RBVariableNode named: 'thisContext')
		selector: #receiver 
	
]

{ #category : #generate }
RFEReceiverReification >> preamble: aNode [
	^aNode isMessage 
		ifTrue: [self preambleForMessage: aNode]
		ifFalse: [{}]
]

{ #category : #generate }
RFEReceiverReification >> preambleForMessage: aNode [
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
