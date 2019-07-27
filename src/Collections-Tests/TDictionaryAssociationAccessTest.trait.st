"
A trait for test purposes
"
Trait {
	#name : #TDictionaryAssociationAccessTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #requirements }
TDictionaryAssociationAccessTest >> keyNotIn [
" return a key not included in nonEmpty"
self explicitRequirement
]

{ #category : #requirements }
TDictionaryAssociationAccessTest >> nonEmpty [
^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TDictionaryAssociationAccessTest >> test0FixtureDictionaryAssocitionAccess [
	self nonEmpty.
	self denyEmpty: self nonEmpty.
	self keyNotIn.
	self deny: (self nonEmpty keys includes: self keyNotIn)
]

{ #category : #'tests - dictionary assocition access' }
TDictionaryAssociationAccessTest >> testAssociationAt [
	| collection keyIn result |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.

	result := collection associationAt: keyIn.

	self assert: result key equals: keyIn.
	self assert: result value equals: (collection at: keyIn)
]

{ #category : #'tests - dictionary assocition access' }
TDictionaryAssociationAccessTest >> testAssociationAtError [

	| collection nonExistantKey |
	collection := self nonEmpty.
	nonExistantKey := self keyNotIn .

	self should: [collection associationAt: nonExistantKey] raise: Error. 


]

{ #category : #'tests - dictionary assocition access' }
TDictionaryAssociationAccessTest >> testAssociationAtIfAbsent [

	| collection keyIn result |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.

	result := collection associationAt: keyIn ifAbsent: [888]. 

	self assert: (result key) = keyIn.
	self assert: (result value ) = (collection at: keyIn ).

	self assert: (collection associationAt: self keyNotIn  ifAbsent: [888] ) = 888
]

{ #category : #'tests - dictionary assocition access' }
TDictionaryAssociationAccessTest >> testAssociationAtIfPresent [

	| collection keyIn result |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.

	result := collection associationAt: keyIn ifPresent: [:assoc | {#present. assoc}].
	self
		assert: result isArray;
		assert: result size = 2;
		assert: result first = #present;
		assert: result second key = keyIn;
		assert: result second value = (collection at: keyIn).

	"test that cull: is used to support zero-arg blocks"
	result := collection associationAt: keyIn ifPresent: [#present].
	self assert: result = #present.

	result := collection associationAt: self keyNotIn ifPresent: [:assoc | {#present. assoc}].
	self assert: result isNil.
]

{ #category : #'tests - dictionary assocition access' }
TDictionaryAssociationAccessTest >> testAssociationAtIfPresentifAbsent [

	| collection keyIn result found |
	found := false.
	collection := self nonEmpty.
	keyIn := collection keys anyOne.

	result := collection associationAt: keyIn ifPresent: [found := true ] ifAbsent: [888].
	self assert: found.

	self assert: (collection associationAt: self keyNotIn  ifAbsent: [888] ) = 888.
	
	self assert: (collection associationAt: self keyNotIn  ifPresent: [666] ifAbsent: [888] ) = 888
]
