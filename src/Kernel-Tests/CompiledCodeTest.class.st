Class {
	#name : #CompiledCodeTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #accessing }
CompiledCodeTest >> compiledMethod1 [
	^ self class >> #method1
]

{ #category : #accessing }
CompiledCodeTest >> method1 [
	
	| array |
	array := Array new.
	array at: 1 put: 'Pharo loves tests'.
	#(#add #at: #remove) 
		do: #printOn:
]

{ #category : #accessing }
CompiledCodeTest >> testLiteralsDoesNotContainMethodName [

	self deny: (self compiledMethod1 hasLiteralThorough: #method1)
]

{ #category : #accessing }
CompiledCodeTest >> testLiteralsHasArrayOfLiterals [

	self assert: (self compiledMethod1 hasLiteralThorough: #(#add #at: #remove))
]

{ #category : #accessing }
CompiledCodeTest >> testLiteralsHasByteStringLiteral [

	self assert: (self compiledMethod1 hasLiteralThorough: 'Pharo loves tests')
]

{ #category : #accessing }
CompiledCodeTest >> testLiteralsHasByteSymbolLiteral [

	self assert: (self compiledMethod1 hasLiteralThorough: #printOn:)
]

{ #category : #accessing }
CompiledCodeTest >> testLiteralsHasGlobalVariableLiteral [
	
	self
		assert:
			(self compiledMethod1
				hasLiteralThorough: (self class environment associationAt: #Array))
]
