"
A warning for an undeclared variable in a method of a class
"
Class {
	#name : #UndeclaredVariableWarning,
	#superclass : #Warning,
	#instVars : [
		'name',
		'selector',
		'class'
	],
	#category : #'Compiler-Support'
}

{ #category : #exceptionDescription }
UndeclaredVariableWarning >> defaultAction [
	"The user should be notified of the occurrence of an exceptional occurrence and
	 given an option of continuing or aborting the computation. The description of the
	 occurrence should include any text specified as the argument of the #signal: message."
	
	(selector
		ifNotNil: [ class name, '>>', selector, ' ' ]
		ifNil: [ '' ]) logCr.
	'(' , name , ' is Undeclared) ' logCr.
	^true
]

{ #category : #exceptionDescription }
UndeclaredVariableWarning >> defaultResumeValue [
	"Answer the value that by default should be returned if the exception is resumed"
	^true
]

{ #category : #exceptionDescription }
UndeclaredVariableWarning >> messageText [
	^ String
		streamContents: [ :s | 
			selector
				ifNotNil: [ s
						print: class;
						nextPutAll: '>>';
						print: selector;
						space ]
				ifNil: [ s newLine ].
			s
				nextPut: $(;
				print: name;
				nextPutAll: ' is Undeclared) ' ]
]

{ #category : #'initialize-release' }
UndeclaredVariableWarning >> name: aString selector: aSymbolOrNil class: aBehavior [
	name := aString.
	selector := aSymbolOrNil.
	class := aBehavior
]
