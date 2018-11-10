"
I perform a push.
"
Class {
	#name : #IceTipPushAction,
	#superclass : #IceTipAction,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #executing }
IceTipPushAction >> basicExecute [
	UIManager default 
		informUser: 'Pushing...' 
		during: [ self repository push ].
	Iceberg announcer announce: (IceRepositoryModified for: self repository).
	
]
