"
I am a copy of class ClassDeprecationMetaLinkFunctionalTest. This comment is copied from there, and might not be entirely accurate

This class contains tests
"
Class {
	#name : #ClassDeprecationTest,
	#superclass : #TestCase,
	#instVars : [
		'systemAdditions',
		'wasEnabled',
		'deprecatedClass',
		'referencingClass',
		'referencingOriginalSource',
		'referencingTransformedSource',
		'replacementClass',
		'oldAuthorFullName'
	],
	#category : #'ClassDeprecation-Tests'
}

{ #category : #adding }
ClassDeprecationTest >> addClassNamed: aSymbol [
	| class |
	class := Object
		subclass: aSymbol
		instanceVariableNames: ''
		classVariableNames: ''
		package: self class category.
	systemAdditions push: class.
	^ class
]

{ #category : #adding }
ClassDeprecationTest >> deprecateClass: aClass [
	aClass classSide
		compile:
			'isDeprecated
	^ true'
]

{ #category : #adding }
ClassDeprecationTest >> deprecateClass: aClass transformTo: aString [
	aClass classSide
		compile:
			'isDeprecated
	<transformTo: ''' , aString
				,
					'''>
	^ true'
]

{ #category : #adding }
ClassDeprecationTest >> deprecateNotClass: aClass [
	(aClass classSide >> #isDeprecated) removeFromSystem
]

{ #category : #accessing }
ClassDeprecationTest >> globalNodes: aCompiledMethod [
	^ aCompiledMethod ast allChildren
		select: [ :e | e isVariable and: [ e isGlobal ] ]
]

{ #category : #running }
ClassDeprecationTest >> setUp [
	super setUp.
	oldAuthorFullName := Author fullNamePerSe.
	Author fullName: self class name.
	systemAdditions := Stack new.
	wasEnabled := ClassDeprecationMetaLink enabled.
	ClassDeprecationMetaLink disable.
	deprecatedClass := self
		addClassNamed: #ClassDeprecationForTestDeprecated.
	referencingClass := self
		addClassNamed: #ClassDeprecationForTestReferencing.
	replacementClass := self
		addClassNamed: #ClassDeprecationForTestReplacement.
	self
		deprecateClass: deprecatedClass
		transformTo: replacementClass name.
	referencingOriginalSource := 'references
	^ {ClassDeprecationForTestDeprecated.
	1.
	self class.
	ClassDeprecationForTestDeprecated.
	ClassDeprecationForTestReferencing}'.
	referencingTransformedSource := 'references
	^ {ClassDeprecationForTestReplacement.
	1.
	self class.
	ClassDeprecationForTestReplacement.
	ClassDeprecationForTestReferencing}'.
]

{ #category : #running }
ClassDeprecationTest >> tearDown [
	super tearDown.
	[ systemAdditions isEmpty ]
		whileFalse: [ systemAdditions pop removeFromSystem ].
	ClassDeprecationMetaLink enable: wasEnabled.
	oldAuthorFullName isEmptyOrNil
		ifTrue: [ Author reset ]
		ifFalse: [ Author fullName: oldAuthorFullName ]
]
