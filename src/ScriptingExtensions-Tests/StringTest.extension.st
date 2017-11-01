Extension { #name : #StringTest }

{ #category : #'*ScriptingExtensions-Tests' }
StringTest >> testAsClass [
	self 
		assert: 'String' asClass
		equals: String
]

{ #category : #'*ScriptingExtensions-Tests' }
StringTest >> testAsClassIfAbsent [

	self 
		assert: ('String' asClassIfAbsent: [ self fail ])
		equals: String.
	
	'+' asClassIfAbsent: [ ^ self "success" ].
	self fail.
]

{ #category : #'*ScriptingExtensions-Tests' }
StringTest >> testAsClassIfPresent [
	
	'String' asClassIfPresent: [ :class | 
		self assert: class equals: String ].
	
	'+' asClassIfPresent: [ ^ self fail ].
]
