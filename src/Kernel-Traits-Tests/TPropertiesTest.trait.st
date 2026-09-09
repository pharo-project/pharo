Trait {
	#name : 'TPropertiesTest',
	#category : 'Kernel-Traits-Tests',
	#package : 'Kernel-Traits-Tests'
}

{ #category : 'requirements' }
TPropertiesTest >> classWithProperties [
	^self explicitRequirement 
]

{ #category : 'requirements' }
TPropertiesTest >> keyNotIn [ 
	^self explicitRequirement 
]

{ #category : 'requirements' }
TPropertiesTest >> supportsNilKey [

^true
]

{ #category : 'tests' }
TPropertiesTest >> testPropertyAt [ 
	|class association|
	class := self classWithProperties. 
	association := class properties associations anyOne. 
	self assert: (class propertyAt: association key) equals: association value.
	self should: [ class propertyAt: self keyNotIn ] raise: Error.
]

{ #category : 'tests' }
TPropertiesTest >> testPropertyAtIfAbsent [
	|class association|
	class := self classWithProperties .
	association := class properties associations anyOne.
	self
		assert:
			(class propertyAt: association key ifAbsent: [ Error signal ])
				= association value.
	self
		assert:
			(class propertyAt: self keyNotIn ifAbsent: [ self valueNotIn ])
				= self valueNotIn
]

{ #category : 'tests' }
TPropertiesTest >> testPropertyAtIfAbsentPut [
	"self run: #testAtIfAbsentPut"

	| class association |
	class := self classWithProperties .
	association := class properties associations anyOne.
	self
		assert:
			(class propertyAt: association key ifAbsentPut: [ Error signal ])
				= association value.
	self assert: (class propertyAt: association key) = association value.
	self
		assert:
			(class propertyAt: self keyNotIn ifAbsentPut: [ self valueNotIn ])
				= self valueNotIn.
	self assert: (class propertyAt: self keyNotIn) = self valueNotIn
]

{ #category : 'tests' }
TPropertiesTest >> testPropertyAtIfPresent [
	"self run: #testAtIfPresent"

	| class association arg |
	class := self classWithProperties .
	association := class properties associations anyOne.
	arg := nil.
	self
		assert:
			(class
				propertyAt: association key
				ifPresent: [ :value |
					arg := value.
					#present ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert: (class propertyAt: association key ifPresent: [ #present ]) = #present.
	self
		assert:
			(class
				propertyAt: self keyNotIn
				ifPresent: [ :value | Error signal ]) isNil
]

{ #category : 'tests' }
TPropertiesTest >> testPropertyAtIfPresentIfAbsent [
	"self run: #testAtIfPresentIfAbsent"

	| class association arg |
	class := self classWithProperties .
	association := class properties associations anyOne.
	arg := nil.
	self
		assert:
			(class
				propertyAt: association key
				ifPresent: [ :value |
					arg := value.
					#present ]
				ifAbsent: [ Error signal ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert:
			(class
				propertyAt: association key
				ifPresent: [ #present ]
				ifAbsent: [ Error signal ]) = #present.
	self
		assert:
			(class
				propertyAt: self keyNotIn
				ifPresent: [ :value | Error signal ]
				ifAbsent: [ self valueNotIn ]) = self valueNotIn
]

{ #category : 'tests' }
TPropertiesTest >> testPropertyAtIfPresentIfAbsentPut [
	"self run: #testAtIfPresentIfAbsentPut"

	| class association arg |
	class := self classWithProperties .
	association := class properties associations anyOne.
	arg := nil.
	self
		assert:
			(class
				propertyAt: association key
				ifPresent: [ :value |
					arg := value.
					#present ]
				ifAbsentPut: [ Error signal ]) = #present.
	self assert: arg = association value.

	"ensure cull: is used to support blocks that take no args"
	self
		assert:
			(class
				propertyAt: association key
				ifPresent: [ #present ]
				ifAbsentPut: [ Error signal ]) = #present.
	self
		assert:
			(class
				propertyAt: self keyNotIn
				ifPresent: [ :value | Error signal ]
				ifAbsentPut: [ self valueNotIn ]) = self valueNotIn.
	self assert: (class propertyAt: self keyNotIn) = self valueNotIn
]

{ #category : 'tests' }
TPropertiesTest >> testPropertyAtPutDict [
	"Do not rename to testAtPut, will conflict with TPutBasicTest>>#testAtPut"

	"self run: #testAtPutDict"

	| class keyIn newValue |
	class := self classWithProperties .
	keyIn := class properties keys anyOne.
	newValue := self valueNotIn.

	"use == to ensure the exact object argument to put: is stored and returned"
	self assert: (class propertyAt: keyIn put: newValue) == newValue.
	self assert: (class propertyAt: keyIn) == newValue.

	newValue := self valueNotIn -> self valueNotIn.
	self assert: (class propertyAt: keyIn put: newValue) == newValue.
	self assert: (class propertyAt: keyIn) == newValue.

	newValue := {self valueNotIn.
	self valueNotIn.
	self valueNotIn}.
	self assert: (class propertyAt: self keyNotIn put: newValue) == newValue.
	self assert: (class propertyAt: self keyNotIn) == newValue
]

{ #category : 'tests' }
TPropertiesTest >> testPropertyAtPutNil [
	"self run: #testAtPutNil"

	| class keyIn newValue |
	class := self classWithProperties .
	keyIn := class properties keys anyOne.
	newValue := self valueNotIn.
	self assert: (class propertyAt: nil put: newValue) == newValue.
	self supportsNilKey
		ifTrue: [ self assert: (class propertyAt: nil) == newValue ]
		ifFalse: [ self deny: (class properties includesKey: nil) ].
	self assert: (class propertyAt: keyIn put: nil) isNil.
	self assert: (class propertyAt: keyIn) isNil.
	self assert: (class propertyAt: self keyNotIn put: nil) isNil.
	self assert: (class propertyAt: self keyNotIn) isNil.
	self assert: (class propertyAt: nil put: nil) isNil.
	self supportsNilKey
		ifTrue: [ self assert: (class propertyAt: nil) isNil ]
		ifFalse: [ self deny: (class properties includesKey: nil) ]
]

{ #category : 'requirements' }
TPropertiesTest >> valueNotIn [ 
	^ self explicitRequirement 
]
