Extension { #name : 'StringTest' }

{ #category : '*ScriptingExtensions-Tests' }
StringTest >> testAsClassIfAbsent [

	self
		assert: ('String' asClassIfAbsent: [ self fail ])
		equals: String.

	'+' asClassIfAbsent: [ ^ self "success" ].
	self fail
]

{ #category : '*ScriptingExtensions-Tests' }
StringTest >> testAsClassIfPresent [

	'String' asClassIfPresent: [ :class |
		self assert: class equals: String ].

	'+' asClassIfPresent: [ ^ self fail ]
]

{ #category : '*ScriptingExtensions-Tests' }
StringTest >> testAsClassInEnvironment [

	self
		assert: ('String' asClassInEnvironment: self class environment)
		equals: String
]
