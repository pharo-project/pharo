"
A trait for test purposes
"
Trait {
	#name : #TDictionaryValueAccessTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #requirements }
TDictionaryValueAccessTest >> keyNotIn [
" return a key not included in nonEmpty"
^ self explicitRequirement
]

{ #category : #requirements }
TDictionaryValueAccessTest >> nonEmpty [

^ self explicitRequirement
]

{ #category : #requirements }
TDictionaryValueAccessTest >> supportsNilKey [
	^ true
]

{ #category : #'tests - fixture' }
TDictionaryValueAccessTest >> test0FixtureDictionaryElementAccess [
	| in |
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self keyNotIn.
	in := true.
	self nonEmpty keys detect: [ :key | key = self keyNotIn ] ifNone: [ in := false ].
	self assert: in = false
]

{ #category : #'tests - DictionaryIndexAccessing' }
TDictionaryValueAccessTest >> testAt [
	"self run: #testAt"

	| collection association |
	collection := self nonEmpty.
	association := collection associations anyOne.
	self assert: (collection at: association key) = association value.
	self should: [ collection at: self keyNotIn ] raise: Error
]

{ #category : #'tests - DictionaryIndexAccessing' }
TDictionaryValueAccessTest >> testAtIfAbsent [
	"self run: #testAtIfAbsent"

	| collection association |
	collection := self nonEmpty.
	association := collection associations anyOne.
	self
		assert:
			(collection at: association key ifAbsent: [ Error signal ])
				= association value.
	self
		assert:
			(collection at: self keyNotIn ifAbsent: [ self valueNotIn ])
				= self valueNotIn
]

{ #category : #'tests - DictionaryIndexAccessing' }
TDictionaryValueAccessTest >> testAtIfAbsentPut [
	"self run: #testAtIfAbsentPut"

	| collection association |
	collection := self nonEmpty.
	association := collection associations anyOne.
	self
		assert:
			(collection at: association key ifAbsentPut: [ Error signal ])
				= association value.
	self assert: (collection at: association key) = association value.
	self
		assert:
			(collection at: self keyNotIn ifAbsentPut: [ self valueNotIn ])
				= self valueNotIn.
	self assert: (collection at: self keyNotIn) = self valueNotIn
]

{ #category : #'tests - DictionaryIndexAccessing' }
TDictionaryValueAccessTest >> testAtIfPresent [
	"self run: #testAtIfPresent"

	| collection association arg |
	collection := self nonEmpty.
	association := collection associations anyOne.
	arg := nil.
	self
		assert:
			(collection
				at: association key
				ifPresent: [ :value | 
					arg := value.
					#present ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert: (collection at: association key ifPresent: [ #present ]) = #present.
	self
		assert:
			(collection
				at: self keyNotIn
				ifPresent: [ :value | Error signal ]) isNil
]

{ #category : #'tests - DictionaryIndexAccessing' }
TDictionaryValueAccessTest >> testAtIfPresentIfAbsent [
	"self run: #testAtIfPresentIfAbsent"

	| collection association arg |
	collection := self nonEmpty.
	association := collection associations anyOne.
	arg := nil.
	self
		assert:
			(collection
				at: association key
				ifPresent: [ :value | 
					arg := value.
					#present ]
				ifAbsent: [ Error signal ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert:
			(collection
				at: association key
				ifPresent: [ #present ]
				ifAbsent: [ Error signal ]) = #present.
	self
		assert:
			(collection
				at: self keyNotIn
				ifPresent: [ :value | Error signal ]
				ifAbsent: [ self valueNotIn ]) = self valueNotIn
]

{ #category : #'tests - DictionaryIndexAccessing' }
TDictionaryValueAccessTest >> testAtIfPresentIfAbsentPut [
	"self run: #testAtIfPresentIfAbsentPut"

	| collection association arg |
	collection := self nonEmpty.
	association := collection associations anyOne.
	arg := nil.
	self
		assert:
			(collection
				at: association key
				ifPresent: [ :value | 
					arg := value.
					#present ]
				ifAbsentPut: [ Error signal ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert:
			(collection
				at: association key
				ifPresent: [ #present ]
				ifAbsentPut: [ Error signal ]) = #present.
	self
		assert:
			(collection
				at: self keyNotIn
				ifPresent: [ :value | Error signal ]
				ifAbsentPut: [ self valueNotIn ]) = self valueNotIn.
	self assert: (collection at: self keyNotIn) = self valueNotIn
]

{ #category : #'tests - DictionaryIndexAccessing' }
TDictionaryValueAccessTest >> testAtPutDict [
	"Do not rename to testAtPut, will conflict with TPutBasicTest>>#testAtPut"

	"self run: #testAtPutDict"

	| collection keyIn newValue |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.
	newValue := self valueNotIn.

	"use == to ensure the exact object argument to put: is stored and returned"
	self assert: (collection at: keyIn put: newValue) == newValue.
	self assert: (collection at: keyIn) == newValue.

	newValue := self valueNotIn -> self valueNotIn.
	self assert: (collection at: keyIn put: newValue) == newValue.
	self assert: (collection at: keyIn) == newValue.

	newValue := {self valueNotIn.
	self valueNotIn.
	self valueNotIn}.
	self assert: (collection at: self keyNotIn put: newValue) == newValue.
	self assert: (collection at: self keyNotIn) == newValue
]

{ #category : #'tests - DictionaryIndexAccessing' }
TDictionaryValueAccessTest >> testAtPutNil [
	"self run: #testAtPutNil"

	| collection keyIn newValue |
	collection := self nonEmpty.
	keyIn := collection keys anyOne.
	newValue := self valueNotIn.
	self assert: (collection at: nil put: newValue) == newValue.
	self supportsNilKey
		ifTrue: [ self assert: (collection at: nil) == newValue ]
		ifFalse: [ self deny: (collection includesKey: nil) ].
	self assert: (collection at: keyIn put: nil) isNil.
	self assert: (collection at: keyIn) isNil.
	self assert: (collection at: self keyNotIn put: nil) isNil.
	self assert: (collection at: self keyNotIn) isNil.
	self assert: (collection at: nil put: nil) isNil.
	self supportsNilKey
		ifTrue: [ self assert: (collection at: nil) isNil ]
		ifFalse: [ self deny: (collection includesKey: nil) ]
]

{ #category : #requirements }
TDictionaryValueAccessTest >> valueNotIn [
	self explicitRequirement
]
