Extension { #name : 'SymbolTest' }

{ #category : '*ScriptingExtensions-Tests' }
SymbolTest >> testAsClassIfAbsent [

	self
		assert: (#Symbol asClassIfAbsent: [ self fail ])
		equals: Symbol.

	#+ asClassIfAbsent: [ ^ self "success" ].
	self fail
]

{ #category : '*ScriptingExtensions-Tests' }
SymbolTest >> testAsClassIfPresent [

	#Symbol asClassIfPresent: [ :class |
		self assert: class equals: Symbol ].

	#'+' asClassIfPresent: [ ^ self fail ]
]

{ #category : '*ScriptingExtensions-Tests' }
SymbolTest >> testAsClassInEnvironment [

	self
		assert: (#Symbol asClassInEnvironment: self class environment)
		equals: Symbol
]
