"
I am exception which used to break command execution. 
Usually I am signalled during command preparation when user declines UI request 
"
Class {
	#name : 'CmdCommandAborted',
	#superclass : 'Error',
	#category : 'Commander-Core',
	#package : 'Commander-Core'
}

{ #category : 'notification' }
CmdCommandAborted >> notifyUserOfCommand: aCommand [
]
