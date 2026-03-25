"
Intialized Class Variables define a default value as part of their definition.

The default parameter can be literal or a block.

There are two subclasses: setting the default value on class creation and lazy initialization on read.
"
Class {
	#name : 'AbstractInitializedClassVariable',
	#superclass : 'ClassVariable',
	#instVars : [
		'default'
	],
	#category : 'VariablesLibrary-ClassVariables',
	#package : 'VariablesLibrary',
	#tag : 'ClassVariables'
}

{ #category : 'testing' }
AbstractInitializedClassVariable class >> isAbstract [

	^self == AbstractInitializedClassVariable
]

{ #category : 'accessing' }
AbstractInitializedClassVariable >> default [
	^ default
]

{ #category : 'accessing' }
AbstractInitializedClassVariable >> default: anObject [
	default := anObject
]

{ #category : 'printing' }
AbstractInitializedClassVariable >> definitionOn: aStream [

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

{ #category : 'testing' }
AbstractInitializedClassVariable >> hasSameDefinitionAs: otherVariable [
	
 	^(super hasSameDefinitionAs: otherVariable) 
		and: [ default = otherVariable default ]
]

{ #category : 'printing' }
AbstractInitializedClassVariable >> printOn: aStream [
	aStream
		store: self class name;
		nextPutAll: '(';
		nextPutAll: self name;
		nextPutAll: ', ';
		nextPutAll: ' default: ';
		print: default;
		nextPutAll: ')'
]
