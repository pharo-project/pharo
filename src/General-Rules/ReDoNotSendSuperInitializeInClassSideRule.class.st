"
Don't send super initialize in class side because the class initialize methods of your superclass  may maintain state level shared state and it can be dangerous to automatically reinitialize it. 

For example, 

[[[  
ZnServer class>>initialize
	ManagedServers := IdentitySet new.
	AlwaysRestart := true.
	Smalltalk addToStartUpList: self.
	Smalltalk addToShutDownList: self
]]]
"
Class {
	#name : 'ReDoNotSendSuperInitializeInClassSideRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'accessing' }
ReDoNotSendSuperInitializeInClassSideRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReDoNotSendSuperInitializeInClassSideRule class >> ruleName [
	^ 'Class-side #initialize should not send "super initialize".'
]

{ #category : 'accessing' }
ReDoNotSendSuperInitializeInClassSideRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'DoNotSendSuperInitializeInClassSideRule'
]

{ #category : 'initialization' }
ReDoNotSendSuperInitializeInClassSideRule >> initialize [
	super initialize.
	self
		replace: 'super initialize'
		with: ''
]

{ #category : 'testing' }
ReDoNotSendSuperInitializeInClassSideRule >> shouldCheckMethod: aMethod [

	^ aMethod selector = #initialize and: [ aMethod methodClass isMeta ]
]
