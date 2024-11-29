Class {
	#name : 'MCLoadProtocolCommand',
	#superclass : 'MCSnapshotBrowserCommand',
	#category : 'MonticelloGUI-Commands',
	#package : 'MonticelloGUI',
	#tag : 'Commands'
}

{ #category : 'default' }
MCLoadProtocolCommand class >> defaultName [

	^ 'Load protocol...'
]

{ #category : 'testing' }
MCLoadProtocolCommand >> canBeExecuted [ 

	^ self selectedProtocol notNil
]

{ #category : 'executing' }
MCLoadProtocolCommand >> execute [
	"Load the selected protocol"

	self selectedProtocol load
]

{ #category : 'accessing - selection' }
MCLoadProtocolCommand >> selectedProtocol [

	^ context protocolSelection 
]
