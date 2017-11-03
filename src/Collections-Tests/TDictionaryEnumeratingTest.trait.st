"
A trait for test purposes
"
Trait {
	#name : #TDictionaryEnumeratingTest,
	#category : #Collections-Tests-Unordered
}

{ #category : #requirements }
TDictionaryEnumeratingTest >> emptyDict [
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryEnumeratingTest >> newEmptyDict [
	self explicitRequirement
]

{ #category : #requirements }
TDictionaryEnumeratingTest >> nonEmptyDict [
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TDictionaryEnumeratingTest >> test0FixtureDictionaryEnumeratingTest [
	self nonEmptyDict.
	self deny: self nonEmptyDict isEmpty
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testAssociationsDo [

	| collection keys |
	collection := self nonEmptyDict .

	keys := OrderedCollection new.
	
	collection associationsDo: [ :assoc | 
		keys add: assoc key.
		self assert: ( collection at: assoc key ) = assoc value.		
		].
	
	collection keys do: [:key | self assert: ( keys occurrencesOf: key ) = (collection keys occurrencesOf: key)].
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testAssociationsSelect [
	| collection keys result |
	collection := self nonEmptyDict .
	keys := OrderedCollection new.
	result := collection associationsSelect: [ :assoc  | 
		keys add: assoc key.
		true].
	
	collection keys do: [ :key | self assert: (collection keys occurrencesOf: key) = (keys occurrencesOf: key)].
	self assert: result = collection.
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testCollect [
	| collection values result |
	collection := self nonEmptyDict .
	values := OrderedCollection new.
	result := collection collect: [ :value  | 
		values add: value.
		].
	
	collection values do: [ :value | self assert: (collection values occurrencesOf: value) = (values occurrencesOf: value)].
	self assert: result = collection.
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testDo [
	| t collection |
	collection := self nonEmptyDict .
	t := OrderedCollection new.
	collection do: [:
		value | t add: value
		].
	
	t do: [ :each | self assert: (t occurrencesOf: each ) = ( collection values occurrencesOf: each) ].
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testKeysAndValuesDo [

	
	| collection keys |
	collection := self nonEmptyDict .
	keys := OrderedCollection new.
	collection keysAndValuesDo: [ :key :value | 
		keys add: key.
		self assert: (collection at: key) = value ].
	
	collection keys do: [ :key | self assert: (collection keys occurrencesOf: key) = (keys occurrencesOf: key)]
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testKeysDo [
	| collection keys |
	collection := self nonEmptyDict .
	keys := OrderedCollection new.
	collection keysDo: [ :key  | 
		keys add: key.
		].
	
	collection keys do: [ :key | self assert: (collection keys occurrencesOf: key) = (keys occurrencesOf: key)]
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testReject [
	"Ensure that Dictionary>>reject: answers a dictionary not something else"
	
	| collection result |
	collection := self nonEmptyDict .
	result := collection reject: [ :each | false].
	
	self assert: result = collection. 
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testSelect [
	| collection values result |
	collection := self nonEmptyDict .
	values := OrderedCollection new.
	result := collection select: [ :value  | 
		values add: value.
		true].
	
	collection values do: [ :value| self assert: (collection values occurrencesOf: value) = (values occurrencesOf: value)].
	self assert: result = collection.
]

{ #category : #'tests - dictionnary enumerating' }
TDictionaryEnumeratingTest >> testValuesDo [
	| collection values |
	collection := self nonEmptyDict .
	values := OrderedCollection new.
	collection valuesDo: [ :value  | 
		values add: value.
		].
	
	collection values do: [ :value | self assert: (collection values occurrencesOf: value) = (values occurrencesOf: value)]
]
