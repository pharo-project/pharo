"
SUnit tests for SystemEnvironment
"
Class {
	#name : 'SystemEnvironmentTest',
	#superclass : 'DictionaryTest',
	#category : 'System-Support-Tests-Utilities',
	#package : 'System-Support-Tests',
	#tag : 'Utilities'
}

{ #category : 'building suites' }
SystemEnvironmentTest class >> shouldInheritSelectors [

	^ true
]

{ #category : 'requirements' }
SystemEnvironmentTest >> associationWithKeyAlreadyInToAdd [
	" return an association that will be used to add to nonEmptyDict (the key of this association is already included in nonEmptyDict)"

	^ GlobalVariable key: self nonEmptyDict keys anyOne value: valueNotIn
]

{ #category : 'requirements' }
SystemEnvironmentTest >> canBeUnhealthy [
	"uses GlobalVariables instead of associations"

	^ false
]

{ #category : 'coverage' }
SystemEnvironmentTest >> classToBeTested [

	^ SystemEnvironment
]

{ #category : 'requirements' }
SystemEnvironmentTest >> elementToAdd [
	" return an element of type 'nonEmpy' elements'type'"

	^ GlobalVariable key: #u value: 5
]

{ #category : 'running' }
SystemEnvironmentTest >> setUp [

	super setUp.

	associationNotIn := GlobalVariable key: keyNotIn value: valueNotIn
]

{ #category : 'requirements' }
SystemEnvironmentTest >> supportsNilKey [

	^ false
]

{ #category : 'tests - DictionaryIndexAccessing' }
SystemEnvironmentTest >> testAtPutNil [

	self should: [ self collection at: nil put: nil ] raise: Error
]

{ #category : 'tests' }
SystemEnvironmentTest >> testClassOrTraitNamedReturnsClassForClasses [

	self assert: Object identicalTo: (testingEnvironment classOrTraitNamed: 'Object').
	self assert: Object identicalTo: (testingEnvironment classOrTraitNamed: #Object)
]

{ #category : 'tests' }
SystemEnvironmentTest >> testClassOrTraitNamedReturnsNilForGlobals [

	self assert: nil equals: (testingEnvironment classOrTraitNamed: 'Undeclared').
	self assert: nil equals: (testingEnvironment classOrTraitNamed: #Undeclared)
]

{ #category : 'tests' }
SystemEnvironmentTest >> testEnvironmentOfOrganization [

	| aDictionary |
	aDictionary := SystemEnvironment new.
	self assert: aDictionary organization environment equals: aDictionary
]

{ #category : 'tests' }
SystemEnvironmentTest >> testHasBindingThatBeginsWith [

	super testHasBindingThatBeginsWith.
	self assert: (Smalltalk globals hasBindingThatBeginsWith: 'Obje').
	self deny: (Smalltalk globals hasBindingThatBeginsWith: 'NOTHEREIope')
]

{ #category : 'tests - testing' }
SystemEnvironmentTest >> testIncludesAssociationNoValue [

	| association dictionary |

	association := GlobalVariable key: #key.

	self assert: association value isNil.

	dictionary := self collectionClass new.

	dictionary add: association.

	self assert: (dictionary at: #key) isNil
]

{ #category : 'tests - testing' }
SystemEnvironmentTest >> testIncludesAssociationWithValue [

	| association dictionary |
	association := GlobalVariable key: #key value: 1.
	dictionary := self collectionClass new.
	dictionary add: association.

	self assert: (dictionary at: #key) equals: 1
]

{ #category : 'tests' }
SystemEnvironmentTest >> testOrganizationPerInstance [

	self deny: SystemEnvironment new organization equals: SystemEnvironment new organization
]

{ #category : 'tests' }
SystemEnvironmentTest >> testOtherInstancesOfSystemEnvironmentAsString [

	self deny: SystemEnvironment new asString equals: 'Smalltalk'
]

{ #category : 'tests' }
SystemEnvironmentTest >> testOtherInstancesOfSystemEnvironmentPrintString [

	self deny: SystemEnvironment new printString equals: 'Smalltalk'
]

{ #category : 'tests' }
SystemEnvironmentTest >> testOtherInstancesOfSystemEnvironmentSelfEvaluating [

	self deny: SystemEnvironment new isSelfEvaluating
]

{ #category : 'tests' }
SystemEnvironmentTest >> testPrintOn [

	| printed splittedString |
	printed := String streamContents: [ :stream |
		           self nonEmpty printOn: stream ].

	splittedString := printed findBetweenSubstrings: { ' '. '('. ')' }.

	self assertCollection: splittedString hasSameElements: {
			self prefix.
			self nonEmpty class name.
			'lots'.
			'of'.
			'globals' }
]

{ #category : 'tests' }
SystemEnvironmentTest >> testSetOrganizationSetBackPointer [

	| systemDictionary packageOrganizer |
	systemDictionary := SystemEnvironment new.
	packageOrganizer := PackageOrganizer new.
	systemDictionary organization: packageOrganizer.
	self assert: packageOrganizer environment identicalTo: systemDictionary.
	self assert: systemDictionary organization identicalTo: packageOrganizer
]

{ #category : 'tests' }
SystemEnvironmentTest >> testSmalltalkAsString [

	self assert: Smalltalk asString equals: 'Smalltalk'
]

{ #category : 'tests' }
SystemEnvironmentTest >> testSmalltalkPrintString [

	self assert: Smalltalk printString equals: 'Smalltalk'
]

{ #category : 'tests' }
SystemEnvironmentTest >> testSmalltalkSelfEvaluating [

	self assert: Smalltalk isSelfEvaluating
]

{ #category : 'tests - printing' }
SystemEnvironmentTest >> testStoreOnWithNegativeInteger [

	| dictionary |
	dictionary := { (GlobalVariable key: 'x' value: -1) } as: self classToBeTested.

	self assert: (String streamContents: [ :s | dictionary storeOn: s ]) equals: '((' , self classToBeTested name , ' new) add: (''x''-> -1); yourself)'
]
