Class {
	#name : 'ScriptingExtensionTest',
	#superclass : 'TestCase',
	#category : 'ScriptingExtensions-Tests',
	#package : 'ScriptingExtensions-Tests'
}

{ #category : 'tests' }
ScriptingExtensionTest >> testStringAsClassIfAbsent [

	self
		assert: ('String' asClassIfAbsent: [ self fail ])
		equals: String.

	'+' asClassIfAbsent: [ ^ self "success" ].
	self fail
]

{ #category : 'tests' }
ScriptingExtensionTest >> testStringAsClassIfPresent [

	'String' asClassIfPresent: [ :class |
		self assert: class equals: String ].

	'+' asClassIfPresent: [ ^ self fail ]
]

{ #category : 'tests' }
ScriptingExtensionTest >> testStringAsClassInEnvironment [

	self
		assert: ('String' asClassInEnvironment: self class environment)
		equals: String
]

{ #category : 'tests' }
ScriptingExtensionTest >> testSymbolAsClassIfAbsent [

	self
		assert: (#Symbol asClassIfAbsent: [ self fail ])
		equals: Symbol.

	#+ asClassIfAbsent: [ ^ self "success" ].
	self fail
]

{ #category : 'tests' }
ScriptingExtensionTest >> testSymbolAsClassIfPresent [

	#Symbol asClassIfPresent: [ :class |
		self assert: class equals: Symbol ].

	#'+' asClassIfPresent: [ ^ self fail ]
]

{ #category : 'tests' }
ScriptingExtensionTest >> testSymbolAsClassInEnvironment [

	self
		assert: (#Symbol asClassInEnvironment: self class environment)
		equals: Symbol
]
