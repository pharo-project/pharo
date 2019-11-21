"
SUnit tests for MethodDictionary
"
Class {
	#name : #MethodDictionaryTest,
	#superclass : #TestCase,
	#traits : 'TIncludesTest + TDictionaryKeysValuesAssociationsAccess + TDictionaryRemovingTest',
	#classTraits : 'TIncludesTest classTrait + TDictionaryKeysValuesAssociationsAccess classTrait + TDictionaryRemovingTest classTrait',
	#instVars : [
		'nonEmptyDict'
	],
	#category : #'Collections-Tests-Unordered'
}

{ #category : #helpers }
MethodDictionaryTest >> aValue [

	^ self class >> #aValue
]

{ #category : #helpers }
MethodDictionaryTest >> anIndex [

	^ #aMethodName
]

{ #category : #requirements }
MethodDictionaryTest >> anotherElementNotIn [
" return an element different of 'elementNotIn'  not included in 'nonEmpty' "
	^ self class >> #anotherElementNotIn
]

{ #category : #helpers }
MethodDictionaryTest >> anotherValue [

	^ self class >> #anotherValue
]

{ #category : #assertions }
MethodDictionaryTest >> assertPreservesCapacity: oldDictionary comparedTo: rehashedDictionary [
	self assert: oldDictionary capacity equals: rehashedDictionary capacity
]

{ #category : #assertions }
MethodDictionaryTest >> assertPreservesElements: oldDictionary comparedTo: newDictionary [
	self assert: (oldDictionary keys allSatisfy: [ :key |
		(newDictionary includesKey: key) &
		((newDictionary at: key) == (oldDictionary at: key))
		])
]

{ #category : #requirements }
MethodDictionaryTest >> elementNotIn [
"return an element not included in 'nonEmpty' "

	^ self class >> #elementNotIn
]

{ #category : #requirements }
MethodDictionaryTest >> empty [
	^ MethodDictionary new
]

{ #category : #requirements }
MethodDictionaryTest >> emptyDict [
	^ self empty
]

{ #category : #requirements }
MethodDictionaryTest >> keyNotIn [
" return a key not included in nonEmpty"
^ #bouba
]

{ #category : #requirements }
MethodDictionaryTest >> keyNotInNonEmptyDict [
" return a key not included in nonEmptyDict"
	^ #keyNotInNonEmptyDict
]

{ #category : #setUps }
MethodDictionaryTest >> modifiedMethodDictionaryCopy [
	| copy |
	
	copy := self class methodDict copy.
	copy at: #methodAddedToIncreaseTheDict put: Object >> #=.
	^copy.
]

{ #category : #requirements }
MethodDictionaryTest >> newEmptyDict [
	^ MethodDictionary new
]

{ #category : #requirements }
MethodDictionaryTest >> nonEmpty [
	^ nonEmptyDict
]

{ #category : #requirements }
MethodDictionaryTest >> nonEmptyDict [
	^ nonEmptyDict 
]

{ #category : #requirements }
MethodDictionaryTest >> nonEmptyDifferentFromNonEmptyDict [
	" return a dictionary for which all keys are not included in nonEmptyDict"
	^ MethodDictionary new
		at: #nonEmptyDifferentFromNonEmptyDict put: (self class >> #nonEmptyDifferentFromNonEmptyDict)
]

{ #category : #running }
MethodDictionaryTest >> setUp [
	super setUp.
	nonEmptyDict := MethodDictionary new.
	nonEmptyDict at: #setUp put: (self class >> #setUp).
	nonEmptyDict at: #nonEmpty put: (self class >> #nonEmpty).
]

{ #category : #requirement }
MethodDictionaryTest >> speciesClass [
	^ MethodDictionary
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testAssociationAtExistantKeyReturnsOkAssociation [
	| methodSelector association |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	association := self class methodDict associationAt: methodSelector ifAbsent: [ self error ].

	self assert: association key equals: methodSelector.
	self assert: association value equals: self class >> methodSelector
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testAssociationAtNonexistantKeyExecutesFailBlock [
	| methodSelector result error |
	methodSelector := #inexistant:method:larala:.
	error := #error.
	result := self class methodDict associationAt: methodSelector ifAbsent: [ error ].

	self assert: result equals: error
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testAssociationsDoGoesOverEntireDictionary [
	| associations |
	associations := MethodDictionary new.
	
	self class methodDict associationsDo:[ :association | associations add: association ].
	
	self assertPreservesElements: self class methodDict comparedTo: associations.
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testAtExistantKeyReturnsOkCompiledMethod [
	| methodSelector method |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	method := self class methodDict at: methodSelector ifAbsent: [ self error ].

	self assert: method class equals: CompiledMethod
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testAtNonexistantKeyExecutesFailBlock [
	| methodSelector result error |
	methodSelector := #inexistant:method:larala:.
	error := #error.
	result := self class methodDict at: methodSelector ifAbsent: [ error ].

	self assert: result equals: error
]

{ #category : #tests }
MethodDictionaryTest >> testBehaviorLocalyDefined [
	"method not acquired from a trait or from its superclass"

	self assert: (Object methodDict isKindOf: MethodDictionary).
	self assert: Object selectors asSortedCollection equals: Object selectors asSortedCollection
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testGrowDoublesCapacity [
	| methodDictionary oldCapacity |
	methodDictionary := MethodDictionary new.
	oldCapacity := methodDictionary capacity.
	methodDictionary grow.
	self assert: oldCapacity * 2 equals: methodDictionary capacity
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testGrowPreservesElements [
	| methodDictionary growedMethodDictionary |
	methodDictionary := self class methodDict copy.
	growedMethodDictionary := methodDictionary copy; grow; yourself.

	self assertPreservesElements: methodDictionary comparedTo: growedMethodDictionary.
	self assertPreservesElements: growedMethodDictionary comparedTo: methodDictionary.
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testIdentityKeyAtExistantValueCopyReturnsFailBlock [
	| methodSelector result aMethod error |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	error := #error.

	aMethod := (self class >> methodSelector) copy.
	result := self class methodDict keyAtIdentityValue: aMethod ifAbsent: [ error ].

	self assert: result equals: error
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testIdentityKeyAtExistantValueReturnsOkKey [
	| methodSelector result aMethod |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.

	aMethod := self class >> methodSelector.
	result := self class methodDict keyAtIdentityValue: aMethod ifAbsent: [ self error ].

	self assert: result equals: methodSelector
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testIdentityKeyAtNonExistantValueReturnsFailBlock [
	| methodSelector result error |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	error := #error.

	result := self class methodDict keyAtIdentityValue: self ifAbsent: [ error ].

	self assert: result equals: error
]

{ #category : #tests }
MethodDictionaryTest >> testIncludesKeyLocalyDefined [

	self assert: (self class includesSelector: #testIncludesKeyLocalyDefined).
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testKeyAtExistantValueCopyReturnsOkKey [
	| methodSelector result aMethod |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	aMethod := (self class >> methodSelector) copy.
	result := self class methodDict keyAtValue: aMethod ifAbsent: [ self error ].

	self assert: result equals: methodSelector
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testKeyAtExistantValueReturnsOkKey [
	| methodSelector result aMethod |
	methodSelector := #testAssociationAtExistantKeyReturnsOkAssociation.
	aMethod := self class >> methodSelector.
	result := self class methodDict keyAtValue: aMethod ifAbsent: [ self error ].

	self assert: result equals: methodSelector
]

{ #category : #'tests - others' }
MethodDictionaryTest >> testKeyAtNonexistantValueExecutesFailBlock [
	| methodSelector result error |
	methodSelector := #inexistant:method:larala:.
	error := #error.
	result := self class methodDict keyAtValue: self ifAbsent: [ error ].

	self assert: result equals: error
]

{ #category : #tests }
MethodDictionaryTest >> testKeyAtValueIfAbsentLocalyDefined [
	self
		assert: (self class methodDict keyAtValue: self class >> #testKeyAtValueIfAbsentLocalyDefined ifAbsent: [  ])
		identicalTo: #testKeyAtValueIfAbsentLocalyDefined.

	self assert: (self class methodDict keyAtValue: Object >> #printOn: ifAbsent: [ #notFound ]) identicalTo: #notFound
]

{ #category : #'tests - rehashing' }
MethodDictionaryTest >> testRehashPreservesCapacity [
	| oldDictionary rehashedDictionary |
	oldDictionary := self modifiedMethodDictionaryCopy.

	rehashedDictionary := oldDictionary copy rehash.
	
	self assertPreservesCapacity: oldDictionary comparedTo: rehashedDictionary.
]

{ #category : #'tests - rehashing' }
MethodDictionaryTest >> testRehashPreservesElements [
	| oldDictionary rehashedDictionary |
	oldDictionary := self modifiedMethodDictionaryCopy.
	
	rehashedDictionary := oldDictionary copy rehash.
	
	self assertPreservesElements: oldDictionary comparedTo: rehashedDictionary.
	self assertPreservesElements: rehashedDictionary comparedTo: oldDictionary.
]

{ #category : #'tests - removing' }
MethodDictionaryTest >> testRemoveAllPreservesCapacity [
	| methodSelector dictionary oldSize |
	methodSelector := #testRemoveAllPreservesCapacity.
	dictionary := MethodDictionary new.
	dictionary at: methodSelector put: self class >> methodSelector.

	oldSize := dictionary basicSize.
	dictionary removeAll.
	self assert: oldSize equals: dictionary basicSize
]
