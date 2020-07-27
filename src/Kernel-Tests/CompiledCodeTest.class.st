Class {
	#name : #CompiledCodeTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #examples }
CompiledCodeTest >> compiledMethod1 [
	^ self class >> #method1
]

{ #category : #examples }
CompiledCodeTest >> method1 [
	
	<pragma1: 123 foo: 'bar' >
	| array | 
	array := Array new.
	array at: 1 put: 'Pharo loves tests'.
	#(#add #at: #remove) do: #printOn:
]

{ #category : #'tests - scanning' }
CompiledCodeTest >> testAccessesRef [
	| readMethod writeMethod classVar |
	readMethod := SmalltalkImage class>>#compilerClass.
	writeMethod := SmalltalkImage class>>#compilerClass:.
	classVar := SmalltalkImage classVariableNamed: #CompilerClass.
	
	self assert: (readMethod accessesRef: classVar).
	self assert: (writeMethod accessesRef: classVar).
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testHasLiteral [

	| method |
	method := self class compiler compile: 'method 
		<pragma: #pragma>
		test := 1+2.
		self doIt: [ 
		test := 1 - 2.
		test := #(arrayInBlock).
		self name ].
		^#(#array) '.
	"simpe case: normal send in the method itself"
	self assert: (method hasLiteral: #doIt:).
	"special selector in the method is not found"
	self deny: (method hasLiteral: #+).
	"special selector in the block not found, either"
	self deny: (method hasLiteral: #-).
	"normal selector in the block"
	self assert: (method hasLiteral: #name).
	"we do NOT into arrays, this is different to hasLiteralSuchThat:"
	self flag: #FIXME. "LiteralScanningMissmatch"
	self deny: (method hasLiteral: #array).
	"we do not into arrays in Blocks. this is different to hasLiteralSuchThat:"
	self flag: #FIXME. "LiteralScanningMissmatch"
	self deny: (method hasLiteral: #arrayInBlock).
	"pragmas are NOT covered. this is different to hasLiteralSuchThat: "
	self flag: #FIXME. "LiteralScanningMissmatch"
	self deny: (method hasLiteral: #pragma:).
	"args of pragmas not found, too. this is different to hasLiteralSuchThat:"
	self flag: #FIXME. "LiteralScanningMissmatch"
	self deny: (method hasLiteral: #pragma).
	"the method selector NOT"
	self deny: (method hasLiteral: #method).
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testHasLiteralSuchThat [

	| method |
	method := self class compiler compile: 'method 
		<pragma: #pragma>
		test := 1+2.
		self doIt: [ 
		test := 1 - 2.
		test := #(arrayInBlock).
		self name ].
		^#(#array) '.
	"simpe case: normal send in the method itself"
	self assert: (method hasLiteralSuchThat: [:literal | literal == #doIt:]).
	"special selector in the method is not found"
	self deny: (method hasLiteralSuchThat: [:literal | literal == #+]).
	"special selector in the block not found, either"
	self deny: (method hasLiteralSuchThat: [:literal | literal == #-]).
	"normal selector in the block"
	self assert: (method hasLiteralSuchThat: [:literal | literal == #name]).
	"we look into arrays"
	self assert: (method hasLiteralSuchThat: [:literal | literal == #array]).
	"we look into arrays in Blocks"
	self assert: (method hasLiteralSuchThat: [:literal | literal == #arrayInBlock]).
	"pragmas are covered"
	self assert: (method hasLiteralSuchThat: [:literal | literal == #pragma:]).
	"args of pragmas are found, too"
	self assert: (method hasLiteralSuchThat: [:literal | literal == #pragma]).
	"the method selector NOT"
	self deny: (method hasLiteralSuchThat: [:literal | literal == #method]).
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testHasSelector [

	| method |
	method := self class compiler compile: 'method 
		<pragma: #pragma>
		test := 1+2.
		self doIt: [ 
		test := 1 - 2.
		test := #(arrayInBlock).
		self name ].
		^#(#array) '.
	"simpe case: normal send in the method itself"
	self assert: (method hasSelector: #doIt:).
	"special selector in the method"
	self assert: (method hasSelector: #+).
	"special selector in the block"
	self assert: (method hasSelector: #-).
	"normal selector in the block"
	self assert: (method hasSelector: #name).
	"we look into arrays"
	self assert: (method hasSelector: #array).
	"we look into arrays in Blocks"
	self assert: (method hasSelector: #arrayInBlock).
	"pragmas are covered"
	self assert: (method hasSelector: #pragma:).
	"args of pragmas are found, too"
	self assert: (method hasSelector: #pragma).
	"the method selector NOT"
	self deny: (method hasSelector: #method).
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testHasSelectorSpecialSelectorIndex [

	| method specialIndex |
	method := self class compiler compile: 'method 
		<pragma: #pragma>
		test := 1+2.
		self do: [ 
		test := 1 - 2.
		test := #(arrayInBlock).
		self name ].
		^#(#array) '.
	"simpe case: normal send in the method itself"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #do:.
	self assert: (method hasSelector: #do: specialSelectorIndex: specialIndex).
	"special selector in the method"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #+.
	self assert: (method hasSelector: #+ specialSelectorIndex: specialIndex).
	"special selector in the block"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #-.
	self assert: (method hasSelector: #- specialSelectorIndex: specialIndex).
	"normal selector in the block"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #name.
	self assert: (method hasSelector: #name specialSelectorIndex: specialIndex).
	"we look into arrays"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #array.
	self assert: (method hasSelector: #array specialSelectorIndex: specialIndex).
	"we look into arrays in Blocks"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #arrayInBlock.
	self assert: (method hasSelector: #arrayInBlock specialSelectorIndex: specialIndex).
	"pragmas are covered"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #pragma:.
	self assert: (method hasSelector: #pragma: specialSelectorIndex: specialIndex).
	"args of pragmas are found, too"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #pragma:.
	self assert: (method hasSelector: #pragma: specialSelectorIndex: specialIndex).
	"the method selector NOT"
	specialIndex := Smalltalk specialSelectorIndexOrNil: #method.
	self deny: (method hasSelector: #method specialSelectorIndex: specialIndex).
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testLiteralsDoesNotContainMethodClass [
	
	self
		deny:
			(self compiledMethod1
				refersToLiteral: (self class environment associationAt: self class name asSymbol))
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testLiteralsDoesNotContainMethodName [

	self deny: (self compiledMethod1 refersToLiteral: #method1)
]

{ #category : #'tests - scanning' }
CompiledCodeTest >> testReadsSelf [
	| method |
	method := self class compiler compile: 'method self doit'.
	self assert: method readsSelf.
	method := self class compiler compile: 'method ^self'.
	self assert: method readsSelf.
	method := self class compiler compile: 'method ^thisContext'.
	self deny: method readsSelf.
]

{ #category : #'tests - scanning' }
CompiledCodeTest >> testReadsThisContext [
	| method |
	method := self class compiler compile: 'method ^thisContext'.
	self assert: method readsThisContext.
	method := self class compiler compile: 'method ^self'.
	self deny: method readsThisContext.
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsFalseWhenLiteralNotInMethodPropertiesKey [
	
	[ self compiledMethod1 propertyAt: #Once put: true.
	self deny: (self compiledMethod1 refersToLiteral: #Absent) ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsArrayOfLiterals [

	self assert: (self compiledMethod1 refersToLiteral: #(#add #at: #remove))
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsByteString [

	self assert: (self compiledMethod1 refersToLiteral: 'Pharo loves tests')
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsByteSymbol [

	self assert: (self compiledMethod1 refersToLiteral: #printOn:)
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsContainedInArrayOfLitterals [

	self assert: (self compiledMethod1 refersToLiteral: #add)
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsGlobalVariable [
	
	self assert: (self compiledMethod1
				refersToLiteral: (self class environment associationAt: #Array))
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInMethodPropertiesKey [
	
	[ self compiledMethod1 propertyAt: #Once put: true.
	self assert: (self compiledMethod1 refersToLiteral: #Once) ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInMethodPropertiesValue [
	
	[ self compiledMethod1 propertyAt: #Once put: '123'.
	self assert: (self compiledMethod1 refersToLiteral: '123') ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInMethodPropertiesValueArray [
	
	[ self compiledMethod1 propertyAt: #Once put: #(1 2 3).
	self assert: (self compiledMethod1 refersToLiteral: 1) ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInPragmaArguments [
	
	self assert: (self compiledMethod1 refersToLiteral: 'bar').
	self assert: (self compiledMethod1 refersToLiteral: 123)
]

{ #category : #'tests-literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInPragmaSelector [
	
	self assert: (self compiledMethod1
				refersToLiteral: #pragma1:foo:)
]
