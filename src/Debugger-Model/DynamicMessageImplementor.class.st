"
Called in the debugger to generate methods when you click on the create button
"
Class {
	#name : #DynamicMessageImplementor,
	#superclass : #Object,
	#instVars : [
		'message',
		'class',
		'argumentNames',
		'stream'
	],
	#category : #'Debugger-Model-Utilities'
}

{ #category : #'instance creation' }
DynamicMessageImplementor class >> for: aMessage in: aClass [

	^ self new initializeFor: aMessage in: aClass
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> argumentNameAt: anIndex [

	| argumentName argument |
	
	argument := message arguments at: anIndex.
	argumentName := argument class canonicalArgumentName.
	
	[ argumentNames includes: argumentName ] whileTrue: [ argumentName := argumentName , anIndex asString ].
	argumentNames add: argumentName.

	^ argumentName
]

{ #category : #testing }
DynamicMessageImplementor >> hasParameter: aKeyword [

	^ aKeyword last = $: or: [ message selector isInfix ]
]

{ #category : #initialization }
DynamicMessageImplementor >> initializeFor: aMessage in: aClass [

	message := aMessage.
	class := aClass
]

{ #category : #testing }
DynamicMessageImplementor >> isMessageAGetter [
	
	^ message numArgs = 0 and: [ class instVarNames includes: message selector ]
]

{ #category : #testing }
DynamicMessageImplementor >> isMessageASetter [
	
	^ message numArgs = 1 and: [ class instVarNames includes: message selector allButLast ]
]

{ #category : #testing }
DynamicMessageImplementor >> messageShouldBeImplemented [
	
	^ (self isMessageAGetter | self isMessageASetter) not
]

{ #category : #evaluating }
DynamicMessageImplementor >> value [

	argumentNames := Set new.
	stream := WriteStream on: String new.
	self writeSourceCode.
	
	^ stream contents
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> write: aKeyword of: anIndex [

	stream nextPutAll: aKeyword.
	self writeArgumentNameIfNecessaryOf: aKeyword at: anIndex
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeArgumentNameAt: anIndex [

	| argumentName |
	
	argumentName := self argumentNameAt: anIndex.
	stream
		nextPutAll: ' ';
		nextPutAll: argumentName;
		space
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeArgumentNameIfNecessaryOf: aKeyword at: anIndex [

	(self hasParameter: aKeyword) ifTrue: [ self writeArgumentNameAt: anIndex ]
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeGetterSourceCode [

	stream 
		cr;
		tab;
		nextPutAll: '^ ';
		nextPutAll: message selector 
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeGetterSourceCodeIfNecessary [

	self isMessageAGetter ifTrue: [ self writeGetterSourceCode ]
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeMethodName [

	message selector keywords doWithIndex: [ :aKeyword :anIndex | self write: aKeyword of: anIndex ]
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeSetterSourceCode [

	stream 
		cr;
		tab;
		nextPutAll: message selector allButLast;
		nextPutAll: ' := ';
		nextPutAll: argumentNames anyOne 
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeSetterSourceCodeIfNecessary [

	self isMessageASetter ifTrue: [ self writeSetterSourceCode ]
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeShouldBeImplemented [

	stream
		cr;
		tab;
		nextPutAll: 'self ';
		nextPutAll: #shouldBeImplemented;
		nextPut: $.
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeShouldBeImplementedIfNecessary [

	self messageShouldBeImplemented ifTrue: [ self writeShouldBeImplemented ]
]

{ #category : #'evaluating-private' }
DynamicMessageImplementor >> writeSourceCode [

	self writeMethodName.
	self writeShouldBeImplementedIfNecessary.
	self writeGetterSourceCodeIfNecessary.
	self writeSetterSourceCodeIfNecessary
]
