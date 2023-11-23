"
SUnit tests for SystemDictionary
"
Class {
	#name : 'SystemDictionaryTest',
	#superclass : 'DictionaryTest',
	#category : 'System-Support-Tests-Utilities',
	#package : 'System-Support-Tests',
	#tag : 'Utilities'
}

{ #category : 'building suites' }
SystemDictionaryTest class >> shouldInheritSelectors [

	^ true
]

{ #category : 'requirements' }
SystemDictionaryTest >> associationWithKeyAlreadyInToAdd [
	" return an association that will be used to add to nonEmptyDict (the key of this association is already included in nonEmptyDict)"

	^ GlobalVariable key: self nonEmptyDict keys anyOne value: valueNotIn
]

{ #category : 'requirements' }
SystemDictionaryTest >> canBeUnhealthy [
	"uses GlobalVariables instead of associations"

	^ false
]

{ #category : 'coverage' }
SystemDictionaryTest >> classToBeTested [

	^ SystemDictionary
]

{ #category : 'requirements' }
SystemDictionaryTest >> elementToAdd [
	" return an element of type 'nonEmpy' elements'type'"

	^ GlobalVariable key: #u value: 5
]

{ #category : 'running' }
SystemDictionaryTest >> setUp [

	super setUp.

	associationNotIn := GlobalVariable key: keyNotIn value: valueNotIn
]

{ #category : 'requirements' }
SystemDictionaryTest >> supportsNilKey [

	^ false
]

{ #category : 'tests - DictionaryIndexAccessing' }
SystemDictionaryTest >> testAtPutNil [

	self should: [ self collection at: nil put: nil ] raise: Error
]

{ #category : 'tests' }
SystemDictionaryTest >> testClassOrTraitNamedReturnsClassForClasses [

	self assert: Object identicalTo: (testingEnvironment classOrTraitNamed: 'Object').
	self assert: Object identicalTo: (testingEnvironment classOrTraitNamed: #Object)
]

{ #category : 'tests' }
SystemDictionaryTest >> testClassOrTraitNamedReturnsNilForGlobals [

	self assert: nil equals: (testingEnvironment classOrTraitNamed: 'Undeclared').
	self assert: nil equals: (testingEnvironment classOrTraitNamed: #Undeclared)
]

{ #category : 'tests' }
SystemDictionaryTest >> testEnvironmentOfOrganization [

	| aDictionary |
	aDictionary := SystemDictionary new.
	self assert: aDictionary organization environment equals: aDictionary
]

{ #category : 'tests' }
SystemDictionaryTest >> testHasBindingThatBeginsWith [

	super testHasBindingThatBeginsWith.
	self assert: (Smalltalk globals hasBindingThatBeginsWith: 'Obje').
	self deny: (Smalltalk globals hasBindingThatBeginsWith: 'NOTHEREIope')
]

{ #category : 'tests - testing' }
SystemDictionaryTest >> testIncludesAssociationNoValue [

	| association dictionary |

	association := GlobalVariable key: #key.

	self assert: association value isNil.

	dictionary := self collectionClass new.

	dictionary add: association.

	self assert: (dictionary at: #key) isNil
]

{ #category : 'tests - testing' }
SystemDictionaryTest >> testIncludesAssociationWithValue [

	| association dictionary |
	association := GlobalVariable key: #key value: 1.
	dictionary := self collectionClass new.
	dictionary add: association.

	self assert: (dictionary at: #key) equals: 1
]

{ #category : 'tests' }
SystemDictionaryTest >> testOrganizationPerInstance [

	self deny: SystemDictionary new organization equals: SystemDictionary new organization
]

{ #category : 'tests' }
SystemDictionaryTest >> testOtherInstancesOfSystemDictionaryAsString [

	self deny: SystemDictionary new asString equals: 'Smalltalk'
]

{ #category : 'tests' }
SystemDictionaryTest >> testOtherInstancesOfSystemDictionaryPrintString [

	self deny: SystemDictionary new printString equals: 'Smalltalk'
]

{ #category : 'tests' }
SystemDictionaryTest >> testOtherInstancesOfSystemDictionarySelfEvaluating [

	self deny: SystemDictionary new isSelfEvaluating
]

{ #category : 'tests' }
SystemDictionaryTest >> testPrintOn [

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
SystemDictionaryTest >> testSetOrganizationSetBackPointer [

	| systemDictionary packageOrganizer |
	systemDictionary := SystemDictionary new.
	packageOrganizer := PackageOrganizer new.
	systemDictionary organization: packageOrganizer.
	self assert: packageOrganizer environment identicalTo: systemDictionary.
	self assert: systemDictionary organization identicalTo: packageOrganizer
]

{ #category : 'tests' }
SystemDictionaryTest >> testSmalltalkAsString [

	self assert: Smalltalk asString equals: 'Smalltalk'
]

{ #category : 'tests' }
SystemDictionaryTest >> testSmalltalkPrintString [

	self assert: Smalltalk printString equals: 'Smalltalk'
]

{ #category : 'tests' }
SystemDictionaryTest >> testSmalltalkSelfEvaluating [

	self assert: Smalltalk isSelfEvaluating
]

{ #category : 'tests - printing' }
SystemDictionaryTest >> testStoreOnWithNegativeInteger [

	| dictionary |
	dictionary := { (GlobalVariable key: 'x' value: -1) } as: self classToBeTested.

	self assert: (String streamContents: [ :s | dictionary storeOn: s ]) equals: '((' , self classToBeTested name , ' new) add: (''x''-> -1); yourself)'
]
