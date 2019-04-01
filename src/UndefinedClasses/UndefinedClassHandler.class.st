"
I am an handler for the creation of UndefinedClass
"
Class {
	#name : #UndefinedClassHandler,
	#superclass : #Object,
	#category : #'UndefinedClasses-Core'
}

{ #category : #handle }
UndefinedClassHandler >> handleDefinitionString: aString [
	"transform aString into a parsed object aParser"
	
	^ self handleDefinitionWithParser: (CodeParser new parse: aString)
]

{ #category : #handle }
UndefinedClassHandler >> handleDefinitionWithParser: aParser [

	| newClass |
	self class environment
		at: aParser className
		ifPresent: [ :class | ^ class ].

	UndefinedClass
		createUndefinedClassNamed: aParser superclassName
		package: aParser packageName.
	newClass := Smalltalk compilerClass new
		source: aParser originalNodeSource;
		evaluate.
	^ newClass
]
