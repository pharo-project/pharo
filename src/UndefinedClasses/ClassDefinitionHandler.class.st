"
I am an handler in charge of the creation of Classes from aString
"
Class {
	#name : #ClassDefinitionHandler,
	#superclass : #Object,
	#category : #'UndefinedClasses-Core'
}

{ #category : #compiling }
ClassDefinitionHandler class >> createClassFromDefinitionString: aString [
	"Parse the definition string. Check that the superclass, the traits, the pool dictionaries and the package required exist. If they do not exist, we create them, or we create an *undefined* version for them"
	
	| classDefinition newClass |
	classDefinition := CDClassDefinitionParser parse: aString.
	
	newClass := (self existsClassNamed: classDefinition superclassName)
		ifTrue: [ Smalltalk compiler evaluate: aString ] 
		ifFalse: [ UndefinedClassHandler new handleDefinitionString: aString ].
	
	^ newClass
]

{ #category : #compiling }
ClassDefinitionHandler class >> existsClassNamed: className [
	
	^ className = #nil or: [ self environment includesKey: className ]
]
