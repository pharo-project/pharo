"
I am the baseclass for a ""class definition change"", a change that will create a class or modify the definition of an existing class.
I hold the class definition string and a controller used by the compiler when compiling the class definition for notifying about the
compilation result. My subclass define the kind of class definition I can add, a class, a metaclass a trait or a class trait.

My instance variable ""definedClass"" is the new class object.
"
Class {
	#name : 'RBRefactoryDefinitionChange',
	#superclass : 'RBRefactoryClassChange',
	#instVars : [
		'definition',
		'definedClass',
		'builder'
	],
	#category : 'Refactoring-Changes-Changes-Classes',
	#package : 'Refactoring-Changes',
	#tag : 'Changes-Classes'
}

{ #category : 'instance creation' }
RBRefactoryDefinitionChange class >> make: aBlock [

	| builder |
	builder := ShiftClassBuilder new.

	[ aBlock value: builder ]
		on: Warning
		do: [ :e | e resume ].

	^ self new
		  builder: builder;
		  yourself
]

{ #category : 'comparing' }
RBRefactoryDefinitionChange >> = aDefinitionChange [

	^ self class = aDefinitionChange class and: [ self builder = aDefinitionChange builder ]
]

{ #category : 'accessing' }
RBRefactoryDefinitionChange >> builder [

	^ builder
]

{ #category : 'accessing' }
RBRefactoryDefinitionChange >> builder: aClassBuilder [

	isMeta := false.
	builder := aClassBuilder.
	className := builder name
]

{ #category : 'printing' }
RBRefactoryDefinitionChange >> changeString [

	^ 'Define ' , self displayClassName
]

{ #category : 'accessing' }
RBRefactoryDefinitionChange >> definedClass [

	^ definedClass
]

{ #category : 'accessing' }
RBRefactoryDefinitionChange >> definition [

	^ definition
]

{ #category : 'private' }
RBRefactoryDefinitionChange >> definitionClass [

	self subclassResponsibility
]

{ #category : 'private' }
RBRefactoryDefinitionChange >> generateChanges [

	definedClass := self class classInstaller new makeWithBuilder: self builder
]

{ #category : 'comparing' }
RBRefactoryDefinitionChange >> hash [

	^ builder hash
]

{ #category : 'private' }
RBRefactoryDefinitionChange >> namesIn: aString [

	| names scanner token |
	names := OrderedCollection new.
	scanner := RBScanner on: (ReadStream on: aString).
	[ scanner atEnd ] whileFalse: [
		token := scanner next.
		token isIdentifier ifTrue: [ names add: token value ] ].
	^ names asArray
]

{ #category : 'printing' }
RBRefactoryDefinitionChange >> printOn: aStream [

	aStream nextPutAll: builder printString
]
