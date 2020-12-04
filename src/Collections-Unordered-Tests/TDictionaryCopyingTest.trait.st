"
A trait for test purposes
"
Trait {
	#name : #TDictionaryCopyingTest,
	#traits : 'TCloneTest',
	#classTraits : 'TCloneTest classTrait',
	#category : #'Collections-Unordered-Tests-Traits'
}

{ #category : #helper }
TDictionaryCopyingTest >> empty [

	^ self explicitRequirement
]

{ #category : #requirements }
TDictionaryCopyingTest >> emptyDict [
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryCopyingTest >> newEmptyDict [
	self explicitRequirement
]

{ #category : #helper }
TDictionaryCopyingTest >> nonEmpty [

	^ self explicitRequirement
]

{ #category : #requirements }
TDictionaryCopyingTest >> nonEmptyDict [
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryCopyingTest >> nonEmptyDifferentFromNonEmptyDict [
" return a dictionary for which all keys are not included in nonEmptyDict"
self explicitRequirement.
]

{ #category : #'tests - fixture' }
TDictionaryCopyingTest >> test0FixtureCloneTest [
	self nonEmpty.
	self denyEmpty: self nonEmpty.
	self empty.
	self assertEmpty: self empty
]

{ #category : #'tests - fixture' }
TDictionaryCopyingTest >> test0FixtureDictionaryCopyingTest [
	| duplicateKey |
	self nonEmptyDict.
	self denyEmpty: self nonEmptyDict.
	self nonEmptyDifferentFromNonEmptyDict.
	self denyEmpty: self nonEmptyDifferentFromNonEmptyDict.
	duplicateKey := true.
	self nonEmptyDict keys detect: [ :key | self nonEmptyDifferentFromNonEmptyDict includes: key ] ifNone: [ duplicateKey := false ].
	self deny: duplicateKey
]

{ #category : #'tests - copy - clone' }
TDictionaryCopyingTest >> testCopyCreatesNewObject [
	
	| copy | 
	copy := self nonEmpty copy.
	self deny: self nonEmpty == copy.
	
]

{ #category : #'tests - copy - clone' }
TDictionaryCopyingTest >> testCopyEmpty [
	self assertEmpty: self empty copy
]

{ #category : #'tests - copy - clone' }
TDictionaryCopyingTest >> testCopyNonEmpty [
	| copy |
	copy := self nonEmpty copy.
	self denyEmpty: copy.
	self assert: copy size = self nonEmpty size.
	self nonEmpty do: [ :each | copy includes: each ]
]

{ #category : #'tests - copying' }
TDictionaryCopyingTest >> testDictionaryConcatenationWithCommonKeys [

	| dictionary1 dictionary2 result |
	dictionary1 := self nonEmptyDict.
	dictionary2 := self nonEmptyDict.
	result := dictionary1 , dictionary2.
	self assert: result size = ( dictionary2 size).
	
	dictionary2 associationsDo: [ :assoc | self assert: (result at: assoc key) = assoc value ]
]

{ #category : #'tests - copying' }
TDictionaryCopyingTest >> testDictionaryConcatenationWithCommonKeysDifferentValues [

	| dictionary1 dictionary2 result value |
	
	dictionary1 := self nonEmptyDict.
	value := self nonEmptyDifferentFromNonEmptyDict   values anyOne.
	dictionary2 := dictionary1 copy.
	dictionary2 keys do: [ :key | dictionary2 at: key put: value ].
	
	
	result := dictionary1 , dictionary2.
	self assert: result size = ( dictionary2 size).
	
	dictionary2 associationsDo: [ :assoc | self assert: (result at: assoc key) = assoc value ]
]

{ #category : #'tests - copying' }
TDictionaryCopyingTest >> testDictionaryConcatenationWithoutCommonKeys [

	| dictionary1 dictionary2 result |
	dictionary1 := self nonEmptyDict.
	dictionary2 := self nonEmptyDifferentFromNonEmptyDict.
	result := dictionary1 , dictionary2.
	self assert: result size = (dictionary1 size + dictionary2 size).
	dictionary1 associationsDo: [ :assoc | self assert: (result at: assoc key) = assoc value ].
	dictionary2 associationsDo: [ :assoc | self assert: (result at: assoc key) = assoc value ]
]
