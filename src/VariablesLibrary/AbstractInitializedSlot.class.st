"
Intialized Slots define a default value as part of their definition.

the default parameter can be literal or a block (which is evaluated on init). If the block has a parameter this will be set to the instance.

There are two subclasses: setting the default value on instantion and lazy initialization on read.
"
Class {
	#name : 'AbstractInitializedSlot',
	#superclass : 'InstanceVariableSlot',
	#instVars : [
		'default'
	],
	#category : 'VariablesLibrary-Slots',
	#package : 'VariablesLibrary',
	#tag : 'Slots'
}

{ #category : 'testing' }
AbstractInitializedSlot class >> isAbstract [

	^ self == AbstractInitializedSlot
]

{ #category : 'comparing' }
AbstractInitializedSlot >> = other [
	^ super = other and: [default = other default]
]

{ #category : 'accessing' }
AbstractInitializedSlot >> default [
	^default
]

{ #category : 'accessing' }
AbstractInitializedSlot >> default: anObject [
	"this needs to be a literal or clean block"
	(anObject isLiteral or: [ anObject isBlock and: [anObject isClean]])
		ifFalse: [ self error: 'default value has to be a literal or clean block' ].
	default := anObject
]

{ #category : 'printing' }
AbstractInitializedSlot >> definitionOn: aStream [

	aStream
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' default: '.

	"If we have a block, then we need the source code, and thus depend on the compiler/or some source mapping.
	Make the dependency explicit!"
	default isBlock
		ifTrue: [ aStream nextPutAll: default sourceNode formattedCode ]
		ifFalse: [ default printOn: aStream ]
]

{ #category : 'comparing' }
AbstractInitializedSlot >> hasSameDefinitionAs: otherVariable [
	
 	^(super hasSameDefinitionAs: otherVariable) 
		and: [ default = otherVariable default ]
]

{ #category : 'comparing' }
AbstractInitializedSlot >> hash [
	^super hash bitXor: default hash
]

{ #category : 'printing' }
AbstractInitializedSlot >> printOn: aStream [
	aStream
		store: self class name;
		nextPutAll: '(';
		nextPutAll: self name;
		nextPutAll: ', ';
		nextPutAll: ' default: ';
		print: default;
		nextPutAll: ')'
]
