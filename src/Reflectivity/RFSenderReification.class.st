"
I reify the sender for message sends and methods
"
Class {
	#name : #RFSenderReification,
	#superclass : #RFReification,
	#category : #Reflectivity-Links
}

{ #category : #'plugin interface' }
RFSenderReification class >> entities [
	^{RBMessageNode . RBMethodNode}
]

{ #category : #'plugin interface' }
RFSenderReification class >> key [
	^#sender
]

{ #category : #generate }
RFSenderReification >> genForRBMessageNode [
	^RBVariableNode named: 'self'
]

{ #category : #generate }
RFSenderReification >> genForRBMethodNode [
	^RBMessageNode 
		receiver: (RBMessageNode 
			receiver: (RBVariableNode named: 'thisContext')
			selector: #sender)
		selector: #receiver.
		
]
