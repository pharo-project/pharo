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
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBDoNotSendSuperInitializeInClassSideRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'DoNotSendSuperInitializeInClassSideRule'
]

{ #category : #hooks }
RBDoNotSendSuperInitializeInClassSideRule >> afterCheck: aNode mappings: mappingDict [
		
	aNode methodNode ifNotNil: [ :methNode |
		methNode compiledMethod ifNotNil: [ :method |
			(method selector = #initialize and: [ 
			method methodClass isMeta ])
				ifTrue: [ ^ true ] ] ].
		
	^ false
	
]

{ #category : #accessing }
RBDoNotSendSuperInitializeInClassSideRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBDoNotSendSuperInitializeInClassSideRule >> initialize [
	super initialize.
	self 
		replace: 'super initialize' 
		with: '' 
		
]

{ #category : #accessing }
RBDoNotSendSuperInitializeInClassSideRule >> name [
	^ 'Class-side #initialize should not send "super initialize".'
]
