"
A trait for test purposes
"
Trait {
	#name : #TDictionaryKeyAccessTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #requirements }
TDictionaryKeyAccessTest >> nonEmptyWithoutEqualsValues [
" return a dictionary that doesn't include equal values'"
^self explicitRequirement
]

{ #category : #'tests - fixture' }
TDictionaryKeyAccessTest >> test0FixtureDictionaryKeyAccess [
	| collection equals |
	self nonEmptyWithoutEqualsValues.
	self deny: self nonEmptyWithoutEqualsValues isEmpty.
	equals := true.
	collection := self nonEmptyWithoutEqualsValues values.
	collection detect: [ :each | (collection occurrencesOf: each) > 1 ] ifNone: [ equals := false ].
	self assert: equals = false.
	self valueNotIn.
	self deny: (self nonEmptyWithoutEqualsValues values includes: self valueNotIn)
]

{ #category : #'tests - dictionary key access' }
TDictionaryKeyAccessTest >> testKeyAtIdentityValue [
	
	
	| dict value result |
	dict := self nonEmpty .
	value := dict values anyOne.
	
	result := dict keyAtIdentityValue: value.
	self assert: (dict at: result) = value.
		
	self should: [dict keyAtIdentityValue: self valueNotIn ] raise: Error
	
	
]

{ #category : #'tests - dictionary key access' }
TDictionaryKeyAccessTest >> testKeyAtIdentityValueIfAbsent [
	"self run: #testKeyAtValue"
	
	| dict value result |
	dict := self nonEmpty .
	value := dict values anyOne.
	
	result := dict keyAtIdentityValue: value ifAbsent: [nil].
	self assert: (dict at: result) = value.
		
	self assert: (dict keyAtIdentityValue: self valueNotIn ifAbsent: [nil] ) = nil. 
	
]

{ #category : #'tests - dictionary key access' }
TDictionaryKeyAccessTest >> testKeyAtValue [
	"self run: #testKeyAtValue"
	
	| dict value result |
	dict := self nonEmpty .
	value := dict values anyOne.
	
	result := dict keyAtValue: value.
	self assert: (dict at: result) = value.
		
	self should: [dict keyAtValue: self valueNotIn ] raise: Error
	
	
]

{ #category : #'tests - dictionary key access' }
TDictionaryKeyAccessTest >> testKeyAtValueIfAbsent [
	"self run: #testKeyAtValue"
	
	| dict value result |
	dict := self nonEmpty .
	value := dict values anyOne.
	
	result := dict keyAtValue: value ifAbsent: [nil].
	self assert: (dict at: result) = value.
		
	self assert: (dict keyAtValue: self valueNotIn ifAbsent: [nil] ) = nil. 
	
	
]

{ #category : #requirements }
TDictionaryKeyAccessTest >> valueNotIn [
" return a value not included in nonEmptyWithoutEqualValues "
^self explicitRequirement
]
