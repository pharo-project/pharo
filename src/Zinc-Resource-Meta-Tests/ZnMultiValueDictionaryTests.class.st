Class {
	#name : #ZnMultiValueDictionaryTests,
	#superclass : #TestCase,
	#category : 'Zinc-Resource-Meta-Tests'
}

{ #category : #testing }
ZnMultiValueDictionaryTests >> testDynamicLimit [
	ZnMaximumNumberOfDictionaryEntries
		value: 10
		during: [ 
			| dictionary |
			dictionary := ZnMultiValueDictionary new.
			self assert: dictionary limit equals: 10.
			1 to: 10 do: [ :each | dictionary at: each put: each ].
			self 
				should: [ dictionary at: 11 put: 11 ]
				raise: ZnTooManyDictionaryEntries ]
]

{ #category : #testing }
ZnMultiValueDictionaryTests >> testMultiValues [
	| dictionary values keys |
	dictionary := ZnMultiValueDictionary new.
	dictionary at: 'foo' add: 1; at: 'foo' add: 2.
	self assert: (dictionary at: 'foo') = #(1 2).
	self assert: dictionary keys asArray = #('foo').
	values := OrderedCollection new.
	keys := OrderedCollection new.
	dictionary keysAndValuesDo: [ :key :value | keys add: key. values add: value ].
	self assert: values = (OrderedCollection with: 1 with: 2).
	self assert: keys = (OrderedCollection with: 'foo' with: 'foo')
	
]

{ #category : #testing }
ZnMultiValueDictionaryTests >> testTooManyEntries [
	| dictionary |
	dictionary := ZnMultiValueDictionary new.
	self
		should: [ 1 to: dictionary limit + 1 do: [ :each | 
						dictionary at: each put: each ] ]
		raise: ZnTooManyDictionaryEntries
]
