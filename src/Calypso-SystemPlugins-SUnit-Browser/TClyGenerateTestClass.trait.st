"
Description
--------------------

I am a trait containing the behavior or retrieving the test class of a class.

If the test class does not exists I am able to generate it.
"
Trait {
	#name : #TClyGenerateTestClass,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Traits'
}

{ #category : #action }
TClyGenerateTestClass >> addNewCommentForTestClass: aClass basedOn: baseClass [
	aClass
		comment:
			(String
				streamContents: [ :stream | 
					| name |
					name := aClass name.
					name first isVowel
						ifTrue: [ stream nextPutAll: 'An ' ]
						ifFalse: [ stream nextPutAll: 'A ' ].
					stream
						nextPutAll: name;
						nextPutAll: ' is a test class for testing the behavior of ';
						nextPutAll: baseClass name ])
]

{ #category : #testing }
TClyGenerateTestClass >> isValidClass: inputClass [
	^ (inputClass isTestCase or: [ inputClass isMeta ]) not
]

{ #category : #action }
TClyGenerateTestClass >> newTestClassCategoryFor: aClass [
	| tag |
	tag := aClass package classTagForClass: aClass.
	^ String
		streamContents: [ :s | 
			s
				nextPutAll: aClass package name;
				nextPutAll: '-Tests'.
			tag isRoot
				ifFalse: [ s
						nextPut: $-;
						nextPutAll: tag name ] ]
]

{ #category : #accessing }
TClyGenerateTestClass >> systemEnvironment [
	^ self explicitRequirement
]

{ #category : #accessing }
TClyGenerateTestClass >> testClassFor: inputClass [
	| className resultClass |
	className := self testClassNameFor: inputClass.
	self systemEnvironment
		classNamed: className
		ifPresent: [ :class | resultClass := class ]
		ifAbsent: [
			(self isValidClass: inputClass) ifFalse: [ ClyInvalidClassForTestClassGeneration signalFor: inputClass ].
			self systemEnvironment ensureExistAndRegisterPackageNamed: inputClass package name asString , '-Tests'.
			resultClass := TestCase
				subclass: className
				instanceVariableNames: ''
				classVariableNames: ''
				package: (self newTestClassCategoryFor: inputClass).
			self addNewCommentForTestClass: resultClass basedOn: inputClass ].
	^ resultClass
]

{ #category : #accessing }
TClyGenerateTestClass >> testClassNameFor: inputClass [
	^ (inputClass name , 'Test') asSymbol
]
