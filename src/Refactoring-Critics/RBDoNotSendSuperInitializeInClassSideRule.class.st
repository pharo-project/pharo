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
	#name : #RBDoNotSendSuperInitializeInClassSideRule,
	#superclass : #RBTransformationRule,
	#category : #Refactoring-Critics-TransformationRules
}

{ #category : #accessing }
RBDoNotSendSuperInitializeInClassSideRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'DoNotSendSuperInitializeInClassSideRule'
]

{ #category : #accessing }
RBDoNotSendSuperInitializeInClassSideRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBDoNotSendSuperInitializeInClassSideRule >> initialize [
	super initialize.
	self rewriteRule 
		replace: '``@.statBefore. super initialize. ``@.statAfter.' 
		with: '``@.statBefore. ``@.statAfter.' 
		when: [ :node | class isMeta and: [ node methodNode selector = #initialize ] ]
]

{ #category : #accessing }
RBDoNotSendSuperInitializeInClassSideRule >> name [
	^ 'Class-side #initialize should not send "super initialize".'
]

{ #category : #accessing }
RBDoNotSendSuperInitializeInClassSideRule >> rationale [
	^ 'Don''t send super initialize in class side because some class initialize of class of the top of the hierarchy should not be reinitialized.'
]
