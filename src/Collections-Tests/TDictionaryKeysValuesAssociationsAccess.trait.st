"
A trait for test purposes
"
Trait {
	#name : #TDictionaryKeysValuesAssociationsAccess,
	#category : 'Collections-Tests-Unordered'
}

{ #category : #requirements }
TDictionaryKeysValuesAssociationsAccess >> nonEmpty [

self explicitRequirement.
]

{ #category : #'tests - fixture' }
TDictionaryKeysValuesAssociationsAccess >> test0FixtureDictionaryKeysValuesAssociationsAccess [
	self nonEmpty.
	self deny: self nonEmpty isEmpty
]

{ #category : #'tests - dictionary keys values associations access' }
TDictionaryKeysValuesAssociationsAccess >> testAssociations [

	| collection result  |
	collection := self nonEmpty .
	result := collection associations.
	
	self assert: result size = collection size.
	result do: [:assoc | self assert: (assoc value) = (collection at: assoc key) ].
	"keys do: [ :key | self assert: ( result at: key ) = ( collection at: key )] ."
	
]

{ #category : #'tests - dictionary keys values associations access' }
TDictionaryKeysValuesAssociationsAccess >> testKeys [
	| collection result |
	collection := self nonEmpty.
	result := collection keys.
	result do: [ :key | collection at: key ].
	self assert: result size = collection size.
	self should: [ result detect: [ :each | (result occurrencesOf: each) > 1 ] ] raise: Error
]

{ #category : #'tests - dictionary keys values associations access' }
TDictionaryKeysValuesAssociationsAccess >> testKeysSortedSafely [
	| collection result |
	collection := self nonEmpty.
	result := collection keysSortedSafely.
	result do: [ :key | collection at: key ].
	self assert: result size = collection size.
	self should: [ result detect: [ :each | (result occurrencesOf: each) > 1 ] ] raise: Error.
	self assert: result asArray isSorted
]

{ #category : #'tests - dictionary keys values associations access' }
TDictionaryKeysValuesAssociationsAccess >> testValues [
	
	| collection result |
	collection := self nonEmpty .
	result := collection values.
	
	self assert: result size = collection size.
	result do: [:each | self assert: (collection occurrencesOf:each ) = (result occurrencesOf: each) ].
	
]
