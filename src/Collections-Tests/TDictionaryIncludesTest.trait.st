"
A trait for test purposes
"
Trait {
	#name : #TDictionaryIncludesTest,
	#category : #Collections-Tests-Unordered
}

{ #category : #requirements }
TDictionaryIncludesTest >> keyNotInNonEmpty [
	" return a key not included in nonEmpty"
	^ self explicitRequirement
]

{ #category : #requirements }
TDictionaryIncludesTest >> nonEmpty [

^self explicitRequirement
]

{ #category : #'tests - fixture' }
TDictionaryIncludesTest >> test0FixtureDictionaryIncludes [
	| in |
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self valueNotInNonEmpty.
	in := false.
	self nonEmpty
		valuesDo: [ :assoc | 
			assoc = self valueNotInNonEmpty
				ifTrue: [ in := true ] ].
	self assert: in = false.
	self keyNotInNonEmpty.
	in := false.
	self nonEmpty
		keysDo: [ :assoc | 
			assoc = self keyNotInNonEmpty
				ifTrue: [ in := true ] ].
	self assert: in = false
]

{ #category : #'tests - dictionary including' }
TDictionaryIncludesTest >> testIncludesAssociation [

	|  nonExistantAssociation associationIn keyIn valueIn |

	keyIn := self nonEmpty keys anyOne.
	valueIn := self nonEmpty values anyOne. 
	nonExistantAssociation := self keyNotInNonEmpty -> self valueNotInNonEmpty .
	associationIn := self nonEmpty associations anyOne.	

	self assert:  (self nonEmpty includesAssociation: associationIn ).
	self deny:  (self nonEmpty includesAssociation: nonExistantAssociation ).
	" testing the case where key is included but not with the same value :"
	self deny: (self nonEmpty includesAssociation: (keyIn-> self valueNotInNonEmpty )).
	" testing the case where value is included but not corresponding key :"
	self deny: (self nonEmpty includesAssociation: (self keyNotInNonEmpty -> valueIn  )).




]

{ #category : #'tests - dictionary including' }
TDictionaryIncludesTest >> testIncludesComportementForDictionnary [
	| valueIn collection keyIn |
	collection := self nonEmpty.
	valueIn := collection values anyOne.
	keyIn := collection keys anyOne.
	self assert: (collection includes: valueIn).
	self deny: (collection includes: self valueNotInNonEmpty).
	" testing that includes take only care of values :"
	self deny: (collection includes: keyIn)
]

{ #category : #'tests - dictionary including' }
TDictionaryIncludesTest >> testIncludesIdentityBasicComportement [

| valueIn collection |
collection := self nonEmpty .
valueIn := collection  values anyOne.

self assert: (collection includesIdentity: valueIn ) .
self deny: (collection includesIdentity: self valueNotInNonEmpty ).
]

{ #category : #'tests - dictionary including' }
TDictionaryIncludesTest >> testIncludesKey [

	| collection keyIn nonExistantKey |

	collection := self nonEmpty .
	keyIn := collection keys anyOne.
	nonExistantKey := self keyNotInNonEmpty. 

	self assert: ( collection includesKey: keyIn ).
	self deny: ( collection includesKey: nonExistantKey ).
]

{ #category : #requirements }
TDictionaryIncludesTest >> valueNotInNonEmpty [
	" return a value not included in nonEmpty"
	^ self explicitRequirement
]
