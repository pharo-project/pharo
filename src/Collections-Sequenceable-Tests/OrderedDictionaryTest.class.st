"
This class tests the OrderedDictionary class. It is separate from DictionaryTest to test that accessing, enumerating, and printing methods preserve the order that keys were inserted in.
"
Class {
	#name : #OrderedDictionaryTest,
	#superclass : #TestCase,
	#instVars : [
		'nonIdentityOrderedAssociations',
		'identityOrderedAssociations'
	],
	#category : #'Collections-Sequenceable-Tests-Ordered'
}

{ #category : #testing }
OrderedDictionaryTest class >> shouldInheritSelectors [

	^ true
]

{ #category : #accessing }
OrderedDictionaryTest >> absentKey [

	^self isTestingIdentityDictionary
		ifTrue: [ self identityAbsentKey ]
		ifFalse: [ self nonIdentityAbsentKey ]
]

{ #category : #accessing }
OrderedDictionaryTest >> absentValue [
	^ 'absentValue'
]

{ #category : #assertions }
OrderedDictionaryTest >> assertDictionary: aFirstDictionary doesNotEqual: aSecondDictionary [
	"Test symmetric inequality"

	self
		deny: aFirstDictionary equals: aSecondDictionary;
		deny: aSecondDictionary equals: aFirstDictionary
]

{ #category : #assertions }
OrderedDictionaryTest >> assertDictionary: aFirstDictionary equals: aSecondDictionary [
	"Test reflixive and symmetric equality."

	self
		assert: aFirstDictionary equals: aFirstDictionary;
		assert: aFirstDictionary equals: aSecondDictionary;
		assert: aSecondDictionary equals: aSecondDictionary;
		assert: aSecondDictionary equals: aFirstDictionary
]

{ #category : #assertions }
OrderedDictionaryTest >> assertIsArray: anArray withElements: aCollection [

	self
		assert: anArray isArray;
		assert: anArray equals: aCollection asArray
]

{ #category : #assertions }
OrderedDictionaryTest >> assertIsDictionary: aFirstDictionary copiedFrom: aSecondDictionary withOrderedAssociations: anAssociationCollection [
	self
		deny: aFirstDictionary identicalTo: aSecondDictionary;
		deny: aFirstDictionary dictionary identicalTo: aSecondDictionary dictionary;
		deny: aFirstDictionary orderedKeys identicalTo: aSecondDictionary orderedKeys.

	"esnure the associations were copied (the keys and values can be shared)"
	aFirstDictionary associations do: [ :each | self deny: (aSecondDictionary associations identityIncludes: each) ].

	self assertIsDictionary: aFirstDictionary withOrderedAssociations: anAssociationCollection
]

{ #category : #assertions }
OrderedDictionaryTest >> assertIsDictionary: anObject withOrderedAssociations: anAssociationCollection [
	"Tests that anObject is an instance of the correct dictionary class
	with the specified ordered associations"

	self
		assert: anObject class identicalTo: self dictionaryClass;
		assert: anObject orderedKeys size >= anAssociationCollection size;
		assert: anObject associations size equals: anAssociationCollection size.

	anAssociationCollection
		withIndexDo: [ :each :i |
			self isTestingIdentityDictionary
				ifTrue: [ self
						assert: (anObject orderedKeys at: i) identicalTo: each key;
						assert: (anObject associations at: i) key identicalTo: each key ]
				ifFalse: [ self
						assert: (anObject orderedKeys at: i) equals: each key;
						assert: (anObject associations at: i) key equals: each key ].
			self assert: (anObject associations at: i) value equals: each value ]
]

{ #category : #assertions }
OrderedDictionaryTest >> assertIsDictionary: anObject withUnorderedAssociations: anAssociationCollection [
	"Tests that anObject is an instance of the correct dictionary class
	with the specified associations, but ignoring the order"

	self
		assert: anObject class identicalTo: self dictionaryClass;
		assert: anObject size equals: anAssociationCollection size.
	anAssociationCollection do: [ :each | self assert: (anObject includesAssociation: each) ]
]

{ #category : #assertions }
OrderedDictionaryTest >> assertKey: aKey wasRemovedfrom: aDictionary [

	self deny: (aDictionary includesKey: aKey).
	aDictionary keys asArray, aDictionary orderedKeys asArray do: [:each |
		self deny:
			(self isTestingIdentityDictionary
				ifTrue: [each == aKey]
				ifFalse: [each = aKey])]
]

{ #category : #accessing }
OrderedDictionaryTest >> changedOrderedAssociations [

	^ self orderedAssociations collect: [:each | each key -> self newValue]
]

{ #category : #accessing }
OrderedDictionaryTest >> changedOrderedAssociationsFirst: anInteger [

	^ self changedOrderedAssociations first: anInteger
]

{ #category : #accessing }
OrderedDictionaryTest >> collectClass [

	^ Array
]

{ #category : #accessing }
OrderedDictionaryTest >> defaultCapacity [

	^ self emptyInternalDictionary capacity
]

{ #category : #accessing }
OrderedDictionaryTest >> dictionaryClass [

	^ OrderedDictionary
]

{ #category : #accessing }
OrderedDictionaryTest >> dictionaryWithOrderedAssociations [

	^ self dictionaryClass newFrom: self orderedAssociations
]

{ #category : #accessing }
OrderedDictionaryTest >> emptyDictionary [

	^ self dictionaryClass new
]

{ #category : #accessing }
OrderedDictionaryTest >> emptyInternalDictionary [

	^ self internalDictionaryClass new
]

{ #category : #accessing }
OrderedDictionaryTest >> identityAbsentKey [

	^ self orderedKeys first copy
]

{ #category : #accessing }
OrderedDictionaryTest >> identityOrderedAssociations [
	"Returns ordered associations to use for identity dictionaries.
	The keys are all #= equal but #== different, so only an
	identity dictionary will be able to distinguish them."
	identityOrderedAssociations
		ifNil: [| key |
			key := 'testKey'.
			identityOrderedAssociations :=
				Array
					with: (key := key copy) -> 'testValue'
					with: (key := key copy) -> 'testValue3'
					with: (key := key copy) -> 'testValue2'
					with: (key := key copy) -> 'testValue4'].
	"Return copies of the associations so they can be safely modified
	in one test without affecting another, but do not copy the keys
	and values"
	^ identityOrderedAssociations collect: [:each | each copy]
]

{ #category : #accessing }
OrderedDictionaryTest >> internalDictionaryClass [

	^self isTestingIdentityDictionary
		ifTrue: [ IdentityDictionary]
		ifFalse: [ Dictionary]
]

{ #category : #accessing }
OrderedDictionaryTest >> internalDictionaryWithAssociations [

	^ self internalDictionaryClass newFrom: self orderedAssociations
]

{ #category : #testing }
OrderedDictionaryTest >> isTestingIdentityDictionary [

	^ false
]

{ #category : #accessing }
OrderedDictionaryTest >> newValue [

	^ 'newValue'
]

{ #category : #accessing }
OrderedDictionaryTest >> nonIdentityAbsentKey [

	^ 'absentKey'
]

{ #category : #accessing }
OrderedDictionaryTest >> nonIdentityOrderedAssociations [
	"Returns ordered associations to use for non-identity dictionaries.
	The keys are all #= and #== different and are returned out of their
	natural sort order."
	nonIdentityOrderedAssociations
		ifNil: [
			nonIdentityOrderedAssociations :=
				Array
					with: 'testKey' -> 'testValue'
					with: 'testKey3' -> 'testValue3'
					with: 'testKey2' -> 'testValue2'
					with: 'testKey4' -> 'testValue4'].
	"return copies of the associations so they can be safely modified
	in one test without affecting another, but do not copy the keys
	and values"
	^ nonIdentityOrderedAssociations collect: [:each | each copy]
]

{ #category : #accessing }
OrderedDictionaryTest >> orderedAssociations [
	^self isTestingIdentityDictionary
		ifTrue: [ self identityOrderedAssociations ]
		ifFalse: [ self nonIdentityOrderedAssociations ]
]

{ #category : #accessing }
OrderedDictionaryTest >> orderedAssociationsAllButFirst: anInteger [

	^ self orderedAssociations allButFirst: anInteger
]

{ #category : #accessing }
OrderedDictionaryTest >> orderedAssociationsFirst: anInteger [

	^ self orderedAssociations first: anInteger
]

{ #category : #accessing }
OrderedDictionaryTest >> orderedKeys [

	^ self orderedAssociations collect: [:each | each key]
]

{ #category : #accessing }
OrderedDictionaryTest >> orderedKeysFirst: anInteger [

	^ self orderedKeys first: anInteger
]

{ #category : #accessing }
OrderedDictionaryTest >> orderedValues [

	^ self orderedAssociations collect: [:each | each value]
]

{ #category : #accessing }
OrderedDictionaryTest >> orderedValuesFirst: anInteger [

	^ self orderedValues first: anInteger
]

{ #category : #accessing }
OrderedDictionaryTest >> otherOrderedDictionaryClasses [

	^ OrderedDictionary withAllSubclasses copyWithout: self dictionaryClass
]

{ #category : #assertions }
OrderedDictionaryTest >> should: aBlock enumerate: aCollection [
	| enumerated |

	enumerated := OrderedCollection new.
	aBlock value: [:each | enumerated add: each].
	self assert: enumerated equals: aCollection asOrderedCollection
]

{ #category : #assertions }
OrderedDictionaryTest >> should: aBlock enumerate: aFirstCollection and: aSecondCollection [
	| firstEnumerated secondEnumerated |

	firstEnumerated := OrderedCollection new.
	secondEnumerated := OrderedCollection new.
	aBlock value: [:first :second |
		firstEnumerated addLast: first.
		secondEnumerated addLast: second].
	self
		assert: firstEnumerated equals: aFirstCollection asOrderedCollection;
		assert: secondEnumerated equals: aSecondCollection asOrderedCollection
]

{ #category : #tests }
OrderedDictionaryTest >> testAdd [
	| dictionary |
	dictionary := self emptyDictionary.
	self orderedAssociations
		withIndexDo: [ :each :i |
			self
				deny: (dictionary includesAssociation: each);
				assert: (dictionary add: each) identicalTo: each;
				assert: (dictionary includesAssociation: each).
			self assertIsDictionary: dictionary withOrderedAssociations: (self orderedAssociationsFirst: i) ].

	"ensure adding the same associations doesn't change the order"
	self orderedAssociations
		reverseDo: [ :each |
			self assert: (dictionary add: each) identicalTo: each.
			self assertIsDictionary: dictionary withOrderedAssociations: self orderedAssociations ].

	self changedOrderedAssociations
		withIndexDo: [ :each :i |
			| old |
			old := self orderedAssociations at: i.
			self
				assert: (dictionary add: each) identicalTo: each;
				assert: (dictionary includesAssociation: each);
				deny: (dictionary includesAssociation: old).
			self assertIsDictionary: dictionary withOrderedAssociations: (self changedOrderedAssociationsFirst: i) , (self orderedAssociationsAllButFirst: i) ]
]

{ #category : #tests }
OrderedDictionaryTest >> testAddAll [
	| dictionary addedAssociations |
	dictionary := self emptyDictionary.
	self orderedAssociations do: [ :each | self deny: (dictionary includesAssociation: each) ].

	addedAssociations := self orderedAssociations.
	self assert: (dictionary addAll: addedAssociations) identicalTo: addedAssociations.
	self assertIsDictionary: dictionary withOrderedAssociations: self orderedAssociations.

	"ensure adding the same associations doesn't change the order"
	addedAssociations := self orderedAssociations reversed.
	self assert: (dictionary addAll: addedAssociations) identicalTo: addedAssociations.
	self assertIsDictionary: dictionary withOrderedAssociations: self orderedAssociations.

	addedAssociations := self changedOrderedAssociations.
	self assert: (dictionary addAll: addedAssociations) identicalTo: addedAssociations.
	self assertIsDictionary: dictionary withOrderedAssociations: self changedOrderedAssociations
]

{ #category : #tests }
OrderedDictionaryTest >> testAssociationAt [
	| dictionary |
	dictionary := self emptyDictionary.
	self orderedAssociations
		do: [ :each |
			self should: [ dictionary associationAt: each key ] raise: Error.

			dictionary add: each.
			self assert: (dictionary associationAt: each key) equals: each ]
]

{ #category : #tests }
OrderedDictionaryTest >> testAssociationAtIfAbsent [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				associationAt: each key
				ifAbsent: [self absentValue]) equals: self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				associationAt: each key
				ifAbsent: [self fail]) equals: each]
]

{ #category : #tests }
OrderedDictionaryTest >> testAssociationAtIfPresent [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				associationAt: each key
				ifPresent: [self fail]) isNil.

		dictionary add: each.
		self assert:
			(dictionary
				associationAt: each key
				ifPresent: [:assoc | self newValue -> assoc]) equals: (self newValue -> each).
		"ensure cull: is used"
		self assert:
			(dictionary
				associationAt: each key
				ifPresent: [self newValue]) equals: self newValue]
]

{ #category : #tests }
OrderedDictionaryTest >> testAssociations [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assertIsArray: dictionary associations
		withElements: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			assertIsArray: dictionary associations
			withElements: (self orderedAssociationsFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testAssociationsDo [

	| dictionary |
	dictionary := self emptyDictionary.
	self
		should: [:block | dictionary associationsDo: block]
		enumerate: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			should: [:block | dictionary associationsDo: block]
			enumerate: (self orderedAssociationsFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testAssociationsSelect [
	| dictionary |

	dictionary := self dictionaryWithOrderedAssociations.
	self orderedAssociations do: [:assoc | | selectedAssociations |
		selectedAssociations := self orderedAssociations copyWithout: assoc.
		self
			assertIsDictionary:
				(dictionary associationsSelect: [:each |
					selectedAssociations includes: each])
			copiedFrom: dictionary
			withOrderedAssociations:
				(self orderedAssociations select: [:each |
					selectedAssociations includes: each])]
]

{ #category : #tests }
OrderedDictionaryTest >> testAt [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self
			should: [dictionary at: each key]
			raise: Error.

		dictionary add: each.
		self assert: (dictionary at: each key) equals: each value]
]

{ #category : #tests }
OrderedDictionaryTest >> testAtIfAbsent [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				at: each key
				ifAbsent: [self absentValue]) equals: self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				at: each key
				ifAbsent: [self fail]) equals: each value]
]

{ #category : #tests }
OrderedDictionaryTest >> testAtIfAbsentPut [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				at: each key
				ifAbsentPut: [each value]) equals: each value.
		self assert:
			(dictionary
				at: each key
				ifAbsentPut: [self fail]) equals: each value.
		self
			assertIsArray: dictionary keys
			withElements: (self orderedKeysFirst: i) ]
]

{ #category : #tests }
OrderedDictionaryTest >> testAtIfPresent [
	| dictionary |
	dictionary := self emptyDictionary.
	self orderedAssociations
		do: [ :each |
			self assert: (dictionary at: each key ifPresent: [ self fail ]) isNil.

			dictionary add: each.
			self assert: (dictionary at: each key ifPresent: [ :value | self newValue -> value ]) equals: self newValue -> each value.
			"ensure cull: is used"
			self assert: (dictionary at: each key ifPresent: [ self newValue ]) equals: self newValue ]
]

{ #category : #tests }
OrderedDictionaryTest >> testAtIfPresentIfAbsent [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				at: each key
				ifPresent: [self fail]
				ifAbsent: [self absentValue]) equals: self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				at: each key
				ifPresent: [:value | self newValue -> value]
				ifAbsent: [self fail]) equals: (self newValue -> each value).
		"ensure cull: is used"
		self assert:
			(dictionary
				at: each key
				ifPresent: [self newValue]
				ifAbsent: [self fail]) equals: self newValue]
]

{ #category : #tests }
OrderedDictionaryTest >> testAtIfPresentIfAbsentPut [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				at: each key
				ifPresent: [ self fail ]
				ifAbsentPut: [ each value ]) equals: each value.
		self assert:
			(dictionary
				at: each key
				ifPresent: [ :it | self newValue -> it ]
				ifAbsentPut: [self fail]) equals: (self newValue -> each value).
		self
			assertIsArray: dictionary keys
			withElements: (self orderedKeysFirst: i) ]
]

{ #category : #tests }
OrderedDictionaryTest >> testAtPut [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				at: each key
				put: each value) equals: each value.
		self
			assertIsDictionary: dictionary
			withOrderedAssociations: (self orderedAssociationsFirst: i)].

	self changedOrderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				at: each key
				put: each value) equals: each value.
		self
			assertIsDictionary: dictionary
			withOrderedAssociations:
				(self changedOrderedAssociationsFirst: i),
				(self orderedAssociationsAllButFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testAtUpdate [
	| dict |
	dict := OrderedDictionary newFrom: {2->10. 100->5} .

	dict at: 2 update: [ :v | v+10 ].
	self assert: (dict at: 2) equals: 20.
	self should: [ dict at: 5 update: [ :v | v+1 ] ] raise: KeyNotFound
]

{ #category : #tests }
OrderedDictionaryTest >> testAtUpdateInitial [
	| dict |
	dict := OrderedDictionary  newFrom: {2->10. 100->5} .
	dict at: 5 update: [ :v | v+1 ] initial: 17.
	dict at: 2 update: [ :v | v+10 ] initial: [0].
	self assert: (dict at: 5) equals: 17.
	self assert: (dict at: 2) equals: 20
]

{ #category : #tests }
OrderedDictionaryTest >> testBindingsDo [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [:block | dictionary bindingsDo: block]
		enumerate: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			should: [:block | dictionary bindingsDo: block]
			enumerate: (self orderedAssociationsFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testCapacity [
	"The current Dictionary implementation allocates more than specified.
	The amount allocated may change in the future but it likely won't ever
	be less than specified, so a >= test is used throughout."

	| defaultCapacity dictionary |

	defaultCapacity := self defaultCapacity.
	dictionary := self dictionaryClass new.
	self assert: dictionary capacity >= defaultCapacity.

	dictionary := self dictionaryClass new: (defaultCapacity / 2) asInteger.
	self assert: dictionary capacity >= (defaultCapacity / 2) asInteger.

	dictionary := self dictionaryClass newFrom: self orderedAssociations.
	self assert: dictionary capacity >= self orderedAssociations size.

	self orderedAssociations size to: 0 by: -1 do: [:i |
		self
			shouldnt: [
				dictionary := self dictionaryClass new: i.
				dictionary addAll: self orderedAssociations]
			raise: Error]
]

{ #category : #tests }
OrderedDictionaryTest >> testCollect [
	| dictionary |

	dictionary := self dictionaryWithOrderedAssociations.
	self
		assertIsDictionary:
			(dictionary collect: [:each | each hash])
		copiedFrom: dictionary
		withOrderedAssociations:
			(self orderedAssociations collect: [:each | each key -> each value hash])
]

{ #category : #tests }
OrderedDictionaryTest >> testCopy [
	| dictionary copy |

	dictionary := self dictionaryWithOrderedAssociations.
	copy := dictionary copy.
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.
	self
		assertIsDictionary: copy
		copiedFrom: dictionary
		withOrderedAssociations: self orderedAssociations
]

{ #category : #tests }
OrderedDictionaryTest >> testCopyEmpty [
	| dictionary copy |

	dictionary := self dictionaryWithOrderedAssociations.
	copy := dictionary copyEmpty.
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.
	self
		assertIsDictionary: copy
		copiedFrom: dictionary
		withOrderedAssociations: #()
]

{ #category : #tests }
OrderedDictionaryTest >> testDeclareFrom [
	| dictionary otherDictionary |
	dictionary := self emptyDictionary.
	otherDictionary := self dictionaryWithOrderedAssociations.
	self orderedKeys do: [ :each | self assert: (dictionary declare: each from: otherDictionary) identicalTo: dictionary ].
	self assertIsDictionary: dictionary withOrderedAssociations: self orderedAssociations.
	self assertEmpty: otherDictionary.

	self orderedKeys
		do: [ :each |
			otherDictionary add: each -> self newValue.
			self assert: (dictionary declare: each from: otherDictionary) identicalTo: dictionary ].
	self assertIsDictionary: dictionary withOrderedAssociations: self orderedAssociations
]

{ #category : #tests }
OrderedDictionaryTest >> testDictionary [
	| dictionary |
	dictionary := self emptyDictionary.
	self
		assert: dictionary dictionary class identicalTo: self internalDictionaryClass;
		assert: dictionary dictionary equals: self emptyInternalDictionary.

	dictionary := self dictionaryWithOrderedAssociations.
	self
		assert: dictionary dictionary class identicalTo: self internalDictionaryClass;
		assert: dictionary dictionary equals: self internalDictionaryWithAssociations
]

{ #category : #tests }
OrderedDictionaryTest >> testDictionaryPublicProtocolCompatibility [

	| dictionary |
	dictionary := self emptyDictionary.
	{
		(dictionary -> Dictionary).
		(dictionary class -> Dictionary class) } do: [ :assoc |
		assoc value protocolNames
			reject: [ :protocol | #( 'private' 'print' 'undeclared' 'copy' 'compar' '*' ) anySatisfy: [ :each | protocol asString beginsWith: each ] ]
			thenDo: [ :protocol | (assoc value selectorsInProtocol: protocol) do: [ :each | self assert: (assoc key respondsTo: each) ] ] ]
]

{ #category : #tests }
OrderedDictionaryTest >> testDo [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [:block | dictionary do: block]
		enumerate: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			should: [:block | dictionary do: block]
			enumerate: (self orderedValuesFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testEquals [
	| dictionaryOne dictionaryTwo |

	dictionaryOne := self emptyDictionary.
	dictionaryTwo := self emptyDictionary.
	self
		assertDictionary: dictionaryOne
		equals: dictionaryTwo.

	"For equality, order will not matter"
	self orderedAssociations
		with: self orderedAssociations reversed
		do: [:firstAssociation :secondAssociation |
			dictionaryOne add: firstAssociation.
			dictionaryTwo add: secondAssociation.
			self
				assertDictionary: dictionaryOne
				doesNotEqual: self emptyDictionary.
			self
				assertDictionary: dictionaryTwo
				doesNotEqual: self emptyDictionary.
			dictionaryOne size < self orderedAssociations size
				ifTrue: [
					self
						assertDictionary: dictionaryOne
						doesNotEqual: dictionaryTwo]].

	self
		assertDictionary: dictionaryOne
		equals: dictionaryTwo
]

{ #category : #tests }
OrderedDictionaryTest >> testEqualsDictionary [
	self
		assertDictionary: self emptyDictionary
		doesNotEqual: self emptyInternalDictionary.

	self
		assertDictionary: self dictionaryWithOrderedAssociations
		doesNotEqual: self internalDictionaryWithAssociations
]

{ #category : #tests }
OrderedDictionaryTest >> testEqualsOtherOrderedDictionaries [
	self otherOrderedDictionaryClasses do: [:each |
		self
			assertDictionary: self emptyDictionary
			doesNotEqual: each new.
		self
			assertDictionary: self dictionaryWithOrderedAssociations
			doesNotEqual: (each newFrom: self orderedAssociations)]
]

{ #category : #tests }
OrderedDictionaryTest >> testHash [
	| dictionary otherDictionary internalDictionary otherInternalDictionary |
	dictionary := self emptyDictionary.
	otherDictionary := self emptyDictionary.
	internalDictionary := self emptyInternalDictionary.
	otherInternalDictionary := self emptyInternalDictionary.
	self assert: dictionary hash equals: otherDictionary hash.
	self orderedAssociations
		do: [ :each |
			dictionary add: each.
			internalDictionary add: each copy.
			"if the internal hashes differ after adding to one, the external should too"
			internalDictionary hash = otherInternalDictionary hash ifFalse: [ self deny: dictionary hash equals: otherDictionary hash ].

			otherDictionary add: each copy.
			otherInternalDictionary add: each copy.
			"should be equal regardless now"
			self assert: dictionary hash equals: otherDictionary hash ]
]

{ #category : #tests }
OrderedDictionaryTest >> testIdentityIndexOfKey [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert: (dictionary identityIndexOfKey: each key) equals: 0.

		dictionary add: each.
		self
			assert: (dictionary identityIndexOfKey: each key) equals: i;
			assert: (dictionary identityIndexOfKey: each key copy) equals: 0]
]

{ #category : #tests }
OrderedDictionaryTest >> testIdentityIndexOfKeyIfAbsent [
	| dictionary |
	dictionary := self emptyDictionary.
	self orderedAssociations
		withIndexDo: [ :each :i |
			self assert: (dictionary identityIndexOfKey: each key ifAbsent: [ self absentValue ]) equals: self absentValue.

			dictionary add: each.
			self assert: (dictionary identityIndexOfKey: each key ifAbsent: [ self fail ]) equals: i.
			self assert: (dictionary identityIndexOfKey: each key copy ifAbsent: [ self absentValue ]) equals: self absentValue ]
]

{ #category : #tests }
OrderedDictionaryTest >> testIncludes [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self deny: (dictionary includes: each value).

		dictionary add: each.
		self assert: (dictionary includes: each value)]
]

{ #category : #tests }
OrderedDictionaryTest >> testIncludesAssociation [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self
			deny: (dictionary includesAssociation: each);
			deny: (dictionary includesAssociation: each key -> each value).

		dictionary add: each.
		self
			assert: (dictionary includesAssociation: each);
			assert: (dictionary includesAssociation: each key -> each value)]
]

{ #category : #tests }
OrderedDictionaryTest >> testIncludesIdentity [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self deny: (dictionary includesIdentity: each value).

		dictionary add: each.
		self
			assert: (dictionary includesIdentity: each value);
			deny: (dictionary includesIdentity: each value copy)]
]

{ #category : #tests }
OrderedDictionaryTest >> testIncludesKey [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self deny: (dictionary includesKey: each key).

		dictionary add: each.
		self assert: (dictionary includesKey: each key)]
]

{ #category : #tests }
OrderedDictionaryTest >> testIndexOfKey [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert: (dictionary indexOfKey: each key) equals: 0.

		dictionary add: each.
		self assert: (dictionary indexOfKey: each key) equals: i]
]

{ #category : #tests }
OrderedDictionaryTest >> testIndexOfKeyIfAbsent [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				indexOfKey: each key
				ifAbsent: [self absentValue]) equals: self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				indexOfKey: each key
				ifAbsent: [self fail]) equals: i]
]

{ #category : #tests }
OrderedDictionaryTest >> testIsDictionary [
	self assert: self dictionaryClass new isDictionary
]

{ #category : #tests }
OrderedDictionaryTest >> testKeyAtIdentityValue [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self
			should: [dictionary keyAtIdentityValue: each value]
			raise: Error.

		dictionary add: each.
		self assert: (dictionary keyAtIdentityValue: each value) equals: each key.
		self
			should: [dictionary keyAtIdentityValue: each value copy]
			raise: Error]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeyAtIdentityValueIfAbsent [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				keyAtIdentityValue: each value
				ifAbsent: [self absentKey]) equals: self absentKey.

		dictionary add: each.
		self assert:
			(dictionary
				keyAtIdentityValue: each value
				ifAbsent: [self fail]) equals: each key.
		self assert:
			(dictionary
				keyAtIdentityValue: each value copy
				ifAbsent: [self absentKey]) equals: self absentKey]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeyAtIndex [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [dictionary keyAtIndex: 0]
		raise: Error.
	self orderedAssociations withIndexDo: [:each :i |
		self
			should: [dictionary keyAtIndex: i]
			raise: Error.

		dictionary add: each.
		self assert: (dictionary keyAtIndex: i) equals: each key]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeyAtIndexIfAbsent [
	| dictionary |

	dictionary := self emptyDictionary.
	self assert:
		(dictionary
			keyAtIndex: 0
			ifAbsent: [self absentKey]) equals: self absentKey.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				keyAtIndex: i
				ifAbsent: [self absentKey]) equals: self absentKey.

		dictionary add: each.
		self assert:
			(dictionary
				keyAtIndex: i
				ifAbsent: [self fail]) equals: each key]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeyAtValue [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self
			should: [dictionary keyAtValue: each value]
			raise: Error.

		dictionary add: each.
		self assert: (dictionary keyAtValue: each value) equals: each key]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeyAtValueIfAbsent [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				keyAtValue: each value
				ifAbsent: [self absentKey]) equals: self absentKey.

		dictionary add: each.
		self assert:
			(dictionary
				keyAtValue: each value
				ifAbsent: [self fail]) equals: each key]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeyForIdentity [
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert: (dictionary keyForIdentity: each value) isNil.

		dictionary add: each.
		self
			assert: (dictionary keyForIdentity: each value) equals: each key;
			assert: (dictionary keyForIdentity: each value copy) isNil]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeys [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assertIsArray: dictionary keys
		withElements: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			assertIsArray: dictionary keys
			withElements: (self orderedKeysFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeysAndValuesDo [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [:block | dictionary keysAndValuesDo: block]
		enumerate: #()
		and: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			should: [:block | dictionary keysAndValuesDo: block]
			enumerate: (self orderedKeysFirst: i)
			and: (self orderedValuesFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeysAndValuesRemove [
	| dictionary |
	dictionary := self dictionaryWithOrderedAssociations.
	self orderedAssociations
		withIndexDo: [ :removedAssociation :i |
			| unremovedAssociations |
			unremovedAssociations := (self orderedAssociationsAllButFirst: i) asOrderedCollection.
			dictionary
				keysAndValuesRemove: [ :key :value |
					(self isTestingIdentityDictionary ifTrue: [ key == removedAssociation key ] ifFalse: [ key = removedAssociation key ])
						ifTrue: [ self assert: value equals: removedAssociation value.
							true ]
						ifFalse: [ | unremovedAssociation |
							unremovedAssociation := unremovedAssociations removeFirst.
							self isTestingIdentityDictionary
								ifTrue: [ self assert: key identicalTo: unremovedAssociation key ]
								ifFalse: [ self assert: key equals: unremovedAssociation key ].
							self assert: value equals: unremovedAssociation value.
							false ] ].
			self assertEmpty: unremovedAssociations.
			self assertKey: removedAssociation key wasRemovedfrom: dictionary ].
	self assertEmpty: dictionary
]

{ #category : #tests }
OrderedDictionaryTest >> testKeysDo [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [:block | dictionary keysDo: block]
		enumerate: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			should: [:block | dictionary keysDo: block]
			enumerate: (self orderedKeysFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testKeysSortedSafely [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assertIsArray: dictionary keysSortedSafely
		withElements: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		"The keys are all strings, so the default #sorted implementation is OK."
		self
			assertIsArray: dictionary keysSortedSafely
			withElements: (self orderedKeysFirst: i) sorted]
]

{ #category : #tests }
OrderedDictionaryTest >> testNewFrom [
	| dictionary |

	dictionary := self dictionaryClass newFrom: self orderedAssociations.
	self
		assertIsDictionary: dictionary
		withOrderedAssociations: self orderedAssociations.
	self
		assertIsDictionary: (self dictionaryClass newFrom: dictionary)
		copiedFrom: dictionary
		withOrderedAssociations: self orderedAssociations.
	self
		assertIsDictionary:
			(self dictionaryClass newFrom: self internalDictionaryWithAssociations)
		withUnorderedAssociations: self orderedAssociations
]

{ #category : #'tests - creation' }
OrderedDictionaryTest >> testNewFromKeysAndValues2 [

	| dict newDict |
	dict := self dictionaryClass new
	       at: #a put: 1;
			 at: #b put: 2;
			 at: #c put: 3; yourself.
	newDict := self dictionaryClass newFromKeys: dict keys andValues: dict values.
	dict keysAndValuesDo: [:k :v|
		self assert: (newDict at: k) equals: v ]
]

{ #category : #tests }
OrderedDictionaryTest >> testNewFromPairs [
	| pairs |

	pairs := OrderedCollection new.
	self orderedAssociations do: [:each |
		pairs
			addLast: each key;
			addLast: each value].

	0 to: pairs size do: [:i |
		self
			assertIsDictionary:
				(self dictionaryClass newFromPairs: (pairs copyFrom: 1 to: i))
			withOrderedAssociations:
				(self orderedAssociationsFirst: (i / 2) floor)]
]

{ #category : #tests }
OrderedDictionaryTest >> testOccurrencesOf [
	| dictionary |

	dictionary := self emptyDictionary.
	self assert: (dictionary occurrencesOf: self newValue) equals: 0.
	self orderedKeys withIndexDo: [:each :i |
		dictionary
			at: each
			put: self newValue.
		self assert: (dictionary occurrencesOf: self newValue) equals: i]
]

{ #category : #tests }
OrderedDictionaryTest >> testRemoveAll [
	| dictionary removedKeys |
	dictionary := self dictionaryWithOrderedAssociations.
	removedKeys := dictionary keys.
	self
		denyEmpty: dictionary;
		assert: dictionary removeAll identicalTo: dictionary;
		assertEmpty: dictionary.
	removedKeys do: [ :each | self assertKey: each wasRemovedfrom: dictionary ]
]

{ #category : #tests }
OrderedDictionaryTest >> testRemoveKey [
	| dictionary |

	dictionary := self dictionaryWithOrderedAssociations.
	self orderedAssociations withIndexDo: [:each :i |
		self assert: (dictionary removeKey: each key) equals: each value.
		self
			assertKey: each key
			wasRemovedfrom: dictionary.
		self
			assertIsDictionary: dictionary
			withOrderedAssociations: (self orderedAssociationsAllButFirst: i).

		self
			should: [dictionary removeKey: each key]
			raise: Error]
]

{ #category : #tests }
OrderedDictionaryTest >> testRemoveKeyIfAbsent [
	| dictionary |
	dictionary := self dictionaryWithOrderedAssociations.
	self orderedAssociations
		withIndexDo: [ :each :i |
			self assert: (dictionary removeKey: each key ifAbsent: [ self fail ]) equals: each value.
			self assertKey: each key wasRemovedfrom: dictionary.
			self assertIsDictionary: dictionary withOrderedAssociations: (self orderedAssociationsAllButFirst: i).

			self assert: (dictionary removeKey: each key ifAbsent: [ self absentValue ]) equals: self absentValue ]
]

{ #category : #tests }
OrderedDictionaryTest >> testRemoveKeys [
	0 to: self orderedAssociations size do: [ :i |
		| dictionary keysToRemove |
		dictionary := self dictionaryWithOrderedAssociations.

		"make it a set to ensure it supports non-Sequenceable collections"
		keysToRemove := (self isTestingIdentityDictionary ifTrue: [ IdentitySet ] ifFalse: [ Set ]) withAll: (self orderedKeysFirst: i).
		self assert: (dictionary removeKeys: keysToRemove) identicalTo: dictionary.
		keysToRemove do: [ :each | self assertKey: each wasRemovedfrom: dictionary ].

		self assertIsDictionary: dictionary withOrderedAssociations: (self orderedAssociationsAllButFirst: i) ]
]

{ #category : #tests }
OrderedDictionaryTest >> testSelect [
	| dictionary |

	dictionary := self dictionaryWithOrderedAssociations.
	self orderedValues do: [:value | | selectedValues |
		selectedValues := self orderedValues copyWithout: value.
		self
			assertIsDictionary:
				(dictionary select: [:each |
					selectedValues includes: each])
			copiedFrom: dictionary
			withOrderedAssociations:
				(self orderedAssociations select: [:each |
					selectedValues includes: each value])]
]

{ #category : #tests }
OrderedDictionaryTest >> testSize [
	| dictionary |

	dictionary := self emptyDictionary.
	self assert: dictionary size equals: 0.

	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self assert: dictionary size equals: i].

	self orderedAssociations size to: 1 by: -1 do: [:i |
		dictionary removeKey: (self orderedKeys at: i).
		self assert: dictionary size equals: (i - 1)]
]

{ #category : #tests }
OrderedDictionaryTest >> testStoreOn [
	| print |
	print := String streamContents: [ :s | self orderedAssociations storeOn: s ].
	self assertDictionary: (self class compiler evaluate: print) equals: self orderedAssociations.
	print := String streamContents: [ :s | self emptyDictionary storeOn: s ].
	self assertDictionary: (self class compiler evaluate: print) equals: self emptyDictionary
]

{ #category : #tests }
OrderedDictionaryTest >> testValues [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assertIsArray: dictionary values
		withElements: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			assertIsArray: dictionary values
			withElements: (self orderedValuesFirst: i)]
]

{ #category : #tests }
OrderedDictionaryTest >> testValuesDo [
	| dictionary |

	dictionary := self emptyDictionary.
	self
		should: [:block | dictionary valuesDo: block]
		enumerate: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			should: [:block | dictionary valuesDo: block]
			enumerate: (self orderedValuesFirst: i)]
]
