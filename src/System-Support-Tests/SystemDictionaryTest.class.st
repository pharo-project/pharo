"
SUnit tests for SystemDictionary
"
Class {
	#name : #SystemDictionaryTest,
	#superclass : #DictionaryTest,
	#category : #'System-Support-Tests'
}

{ #category : #'building suites' }
SystemDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : #requirements }
SystemDictionaryTest >> canBeUnhealthy [
	"uses GlobalVariables instead of associations"
	^ false
]

{ #category : #coverage }
SystemDictionaryTest >> classToBeTested [

	^ SystemDictionary
]

{ #category : #tests }
SystemDictionaryTest >> testClassOrTraitNamedReturnsClassForClasses [
	self assert: Object identicalTo: (testingEnvironment classOrTraitNamed: 'Object').
	self assert: Object identicalTo: (testingEnvironment classOrTraitNamed: #Object)
]

{ #category : #tests }
SystemDictionaryTest >> testClassOrTraitNamedReturnsNilForGlobals [
	self assert: nil equals: (testingEnvironment classOrTraitNamed: 'Undeclared').
	self assert: nil equals: (testingEnvironment classOrTraitNamed: #Undeclared)
]

{ #category : #tests }
SystemDictionaryTest >> testEnvironmentOfOrganization [
	
	| aDictionary |	
	aDictionary := SystemDictionary withOrganizer: SystemOrganizer new.
	self assert: aDictionary organization environment equals: aDictionary
	
]

{ #category : #tests }
SystemDictionaryTest >> testOrganizationPerInstance [
	| aDictionary otherDictionary |
	aDictionary := SystemDictionary withOrganizer: SystemOrganizer new.
	otherDictionary := SystemDictionary withOrganizer: SystemOrganizer new.
	self deny: aDictionary organization equals: otherDictionary organization
]

{ #category : #tests }
SystemDictionaryTest >> testOtherInstancesOfSystemDictionaryAsString [
	self deny: SystemDictionary new asString equals: 'Smalltalk'
]

{ #category : #tests }
SystemDictionaryTest >> testOtherInstancesOfSystemDictionaryPrintString [
	self deny: SystemDictionary new printString equals: 'Smalltalk'
]

{ #category : #tests }
SystemDictionaryTest >> testOtherInstancesOfSystemDictionarySelfEvaluating [
	self deny: SystemDictionary new isSelfEvaluating
]

{ #category : #tests }
SystemDictionaryTest >> testSetOrganizationSetBackPointer [
	| sd so |
	sd := SystemDictionary new.
	so := SystemOrganizer new.
	sd organization: so.
	self assert: so environment identicalTo: sd.
	self assert: sd organization identicalTo: so
]

{ #category : #tests }
SystemDictionaryTest >> testSmalltalkAsString [
	self assert: Smalltalk asString equals: 'Smalltalk'
]

{ #category : #tests }
SystemDictionaryTest >> testSmalltalkPrintString [
	self assert: Smalltalk printString equals: 'Smalltalk'
]

{ #category : #tests }
SystemDictionaryTest >> testSmalltalkSelfEvaluating [
	self assert: Smalltalk isSelfEvaluating
]

{ #category : #tests }
SystemDictionaryTest >> testUnCategorizedMethods [
	| categories slips |
	categories := self categoriesForClass: self targetClass.
	slips := categories select: [ :each | each = #'as yet unclassified' ].
	self assertEmpty: slips
]
