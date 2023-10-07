"
RBRefactoryProtocolChange is responsible for adding or removing protocols in classes in a RB model
"
Class {
	#name : 'RBRefactoryProtocolChange',
	#superclass : 'RBRefactoryClassChange',
	#instVars : [
		'protocol'
	],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'printing' }
RBRefactoryProtocolChange >> displayProtocolName [

	^ protocol
]

{ #category : 'accessing' }
RBRefactoryProtocolChange >> protocol: aString [

	protocol := aString
]
