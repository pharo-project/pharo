"
RBRefactoryProtocolChange is responsible for adding or removing protocols in classes in a RB model
"
Class {
	#name : 'ReProtocolChange',
	#superclass : 'ReClassChange',
	#instVars : [
		'protocol'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'printing' }
ReProtocolChange >> displayProtocolName [

	^ protocol
]

{ #category : 'accessing' }
ReProtocolChange >> protocol: aString [

	protocol := aString
]
