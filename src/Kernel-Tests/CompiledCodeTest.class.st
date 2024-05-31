Class {
	#name : 'CompiledCodeTest',
	#superclass : 'TestCase',
	#instVars : [
		'var'
	],
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'examples' }
CompiledCodeTest >> compiledMethod1 [
	^ self class >> #method1
]

{ #category : 'examples' }
CompiledCodeTest >> method1 [

	<pragma1: 123 foo: 'bar' >
	| array |
	array := Array new.
	array at: 1 put: 'Pharo loves tests'.
	#(#add #at: #remove) do: #printOn:
]

{ #category : 'examples' }
CompiledCodeTest >> methodReturnOne [

	^ 1
]

{ #category : 'tests - scanning' }
CompiledCodeTest >> testAccessesRef [
	| readMethod writeMethod classVar |
	readMethod := SmalltalkImage class>>#compilerClass.
	writeMethod := SmalltalkImage class>>#compilerClass:.
	classVar := SmalltalkImage classVariableNamed: #CompilerClass.

	self assert: (readMethod accessesRef: classVar).
	self assert: (writeMethod accessesRef: classVar)
]

{ #category : 'tests - testing' }
CompiledCodeTest >> testBlockReturnSpecial [
	"we do not compile blocks with quick returns for now"
	self deny: [ ^ self ] compiledBlock isReturnSpecial.
	self deny: [ ^ nil ] compiledBlock isReturnSpecial.
	self deny: [ ^ true ] compiledBlock isReturnSpecial.
	self deny: [ ^ false ] compiledBlock isReturnSpecial.
	self deny: [ ^ -1 ] compiledBlock isReturnSpecial.
	self deny: [ ^ 0 ] compiledBlock isReturnSpecial.
	self deny: [ ^ 1 ] compiledBlock isReturnSpecial.
	self deny: [ ^ 2 ] compiledBlock isReturnSpecial.

	self deny: [ ^ 3 ] compiledBlock isReturnSpecial.
	self deny: [ ^ var ] compiledBlock isReturnSpecial.
	self deny: [ ^ Object ] compiledBlock isReturnSpecial
]

{ #category : 'tests - flag' }
CompiledCodeTest >> testClearFlag [
	| method |
	
	method := self class >> #methodReturnOne.
	
	method clearFlag.
	self deny: method flag.
	method setFlag.
	self assert: method flag.
	method clearFlag.
	self deny: method flag.
]

{ #category : 'tests - scanning' }
CompiledCodeTest >> testEquivalentTo [

	| aClass origin |
	aClass := Object newAnonymousSubclass.
	aClass compile: 'm1 ^ self'.
	origin := aClass >> #m1.
	self assert: (origin equivalentTo: origin).
	self assert: (origin equivalentTo: origin copy).

	aClass compile: 'm1 "with comment" ^ self'.
	self assert: (origin equivalentTo: aClass >> #m1).
	self assert: (origin equivalentTo: (aClass >> #m1) copy).

	aClass compile: 'm2 ^ self'.
	self deny: (origin equivalentTo: aClass >> #m2).

	aClass compile: 'm1 <pragma: 1> ^ self'.
	self deny: (origin equivalentTo: aClass >> #m1).

	origin := aClass >> #m1.
	aClass compile: 'm1 <pragma: 2> ^ self'.
	self deny: (origin equivalentTo: aClass >> #m1)
]

{ #category : 'tests - flag' }
CompiledCodeTest >> testFlag [
	| method |
	
	method := self class >> #methodReturnOne.
	self deny: method flag.
	method setFlag.
	self assert: method flag.
	method clearFlag.
	self deny: method flag.

]

{ #category : 'tests - literals' }
CompiledCodeTest >> testHasLiteral [

	| method |
	method := self class compiler permitFaulty: true; compile: 'method
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
	"pragmas are NOT covered."
	self deny: (method hasLiteral: #pragma:).
	"args of pragmas not found, too."
	self deny: (method hasLiteral: #pragma).
	"the method selector NOT"
	self deny: (method hasLiteral: #method)
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testHasLiteralSuchThat [

	| method |
	method := self class compiler permitFaulty: true; compile: 'method
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
	"pragmas are NOT covered"
	self deny: (method hasLiteralSuchThat: [:literal | literal == #pragma:]).
	"args of pragmas are NOT found, too"
	self deny: (method hasLiteralSuchThat: [:literal | literal == #pragma]).
	"the method selector NOT"
	self deny: (method hasLiteralSuchThat: [:literal | literal == #method])
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testHasSelector [

	| method |
	method := self class compiler permitFaulty: true; compile: 'method
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
	self deny: (method hasSelector: #method)
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testHasSelectorSpecialSelectorIndex [

	| method specialIndex |
	method := self class compiler permitFaulty: true; compile: 'method
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
	self deny: (method hasSelector: #method specialSelectorIndex: specialIndex)
]

{ #category : 'tests - testing' }
CompiledCodeTest >> testHasTemporaries [
	| method block |
	method := self class >> #testHasTemporaries.
	self assert: method hasTemporaries.
	block := [ | b | b := 2  ].
	self assert: block method hasTemporaries.
	block := [  2  ].
	self deny: block method hasTemporaries
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testLiteralsDoNotConsiderTheInnerBlockLiterals [

	| method block |
	method := self class compiler permitFaulty: true; compile: 'method
		<pragma: #pragma>
		test := 1+2.
		Class.
		self doIt: [
		test := 1 - 2.
		test := #(arrayInBlock).
		Object.
		self name ].
		^#(#array) '.
	block := (self class compiler permitFaulty: true; evaluate: '[
		test := 1 - 2.
		test := #(arrayInBlock).
		Object.
		self name ]') compiledBlock.
	self assertCollection: method literals equals: {
			2.
			(UndeclaredVariable key: #test value: nil).
			#runtimeUndeclaredWrite:inContext:.
			(GlobalVariable key: #Class value: Class).
			block.
			#doIt:.
			#( #array )}
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testLiteralsDoesNotContainMethodClass [

	self
		deny:
			(self compiledMethod1
				refersToLiteral: (self class environment associationAt: self class name asSymbol))
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testLiteralsDoesNotContainMethodName [

	self deny: (self compiledMethod1 refersToLiteral: #method1)
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testLiteralsEvenTheOnesInTheInnerBlocks [
	"The behavior is different than literals"
	| method |
	method := self class compiler permitFaulty: true; compile: 'method
		<pragma: #pragma>
		test := 1+2.
		Class.
		self doIt: [
		test := 1 - 2.
		test := #(arrayInBlock).
		Object.
		self name ].
		^#(#array) '.
	"Next assertion is expected: literals have a different behavior more close to the system"
	self deny: method literalsEvenTheOnesInTheInnerBlocks equals: method literals.
	self
		assertCollection: method literalsEvenTheOnesInTheInnerBlocks
		equals: {
				2.
				(UndeclaredVariable key: #test value: nil).
				#runtimeUndeclaredWrite:inContext:.
				(GlobalVariable key: #Class value: Class).
				2.
				(UndeclaredVariable key: #test value: nil).
				#runtimeUndeclaredWrite:inContext:.
				#( #arrayInBlock ).
				(GlobalVariable key: #Object value: Object).
				#name.
				#doIt:.
				#( #array ). }
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testLiteralsEvenTheOnesInTheInnerCleanBlocks [

	"The behavior is different than literals"

	| method |
	method := self class compiler compile:
		          'exampleAccessOuterFromCleanBlock
	<compilerOptions: #(+ optionCleanBlockClosure)>
	| b |
	b := 1.
	^[ thisContext tempNamed: ''b'' ] value'.
	self
		assertCollection: method literalsEvenTheOnesInTheInnerBlocks
		equals: #( 'b' #tempNamed: )
]

{ #category : 'tests - scanning' }
CompiledCodeTest >> testLocalMessages [
	| method |
	method := self class compiler compile: 'method self doit. [Object class]'.
	self assert: method localMessages asArray equals: #(#doit) .
	
]

{ #category : 'tests - scanning' }
CompiledCodeTest >> testMessages [
	| method |
	method := self class compiler compile: 'method self doit. [Object class]'.
	self assertCollection: method messages asArray hasSameElements: #(#doit #class).
]

{ #category : 'tests - testing' }
CompiledCodeTest >> testMethodReturnSpecial [

	| returnSpecialMethod normalMethod |
	returnSpecialMethod := self class >> #methodReturnOne.
	self assert: returnSpecialMethod isReturnSpecial.

	normalMethod := self class >> #method1.
	self deny: normalMethod isReturnSpecial
]

{ #category : 'tests - instance creation' }
CompiledCodeTest >> testNewFrom [ 

	| method copy |
	method := OrderedCollection>>#do:.
	copy := CompiledMethod newFrom: method.
	
	self assert: method header equals: copy header.
	self assert: method bytecodes equals: copy bytecodes.
	self deny: method sourcePointer equals: copy sourcePointer.
	
	method := [ 1 ] method.
	copy := CompiledBlock newFrom: method.
	
	self assert: method header equals: copy header.
	self assert: method bytecodes equals: copy bytecodes
	"blocks have no sourcePointer"
]

{ #category : 'tests - scanning' }
CompiledCodeTest >> testReadsRef [
	| readMethod writeMethod classVar |
	readMethod := SmalltalkImage class>>#compilerClass.
	writeMethod := SmalltalkImage class>>#compilerClass:.
	classVar := SmalltalkImage classVariableNamed: #CompilerClass.

	self assert: (readMethod readsRef: classVar).
	self deny: (writeMethod readsRef: classVar).

	"Special test for a method with super in a full block.
	We push a binding for this class but make sure that it is not the same"
	self deny: ((DelayMutexScheduler>>#unschedule:) readsRef: DelayMutexScheduler binding)
]

{ #category : 'tests - scanning' }
CompiledCodeTest >> testReadsSelf [
	| method |
	method := self class compiler compile: 'method self doit'.
	self assert: method readsSelf.
	method := self class compiler compile: 'method ^self'.
	self assert: method readsSelf.
	method := self class compiler compile: 'method ^thisContext'.
	self deny: method readsSelf.
	"take care! FFI methods have a self send, but they do not have self bytecode after the first call
	Here we test just the case of a FFI method that was not yet called to not make this test depend on FFI
	calls"
	method := self class compiler compile: 'status ^ self ffiCall: #(int cairo_scaled_font_status (self))'.
	self assert: method readsSelf
]

{ #category : 'tests - scanning' }
CompiledCodeTest >> testReadsThisContext [
	| method |
	method := self class compiler compile: 'method ^thisContext'.
	self assert: method readsThisContext.
	method := self class compiler compile: 'method ^self'.
	self deny: method readsThisContext
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsFalseWhenLiteralNotInMethodPropertiesKey [

	[ self compiledMethod1 propertyAt: #Once put: true.
	self deny: (self compiledMethod1 refersToLiteral: #Absent) ]
		ensure: [ self compiledMethod1 removeProperty: #Once ]
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsArrayOfLiterals [

	self assert: (self compiledMethod1 refersToLiteral: #(#add #at: #remove))
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsByteString [

	self assert: (self compiledMethod1 refersToLiteral: 'Pharo loves tests')
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsByteSymbol [

	self assert: (self compiledMethod1 refersToLiteral: #printOn:)
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsContainedInArrayOfLitterals [

	self assert: (self compiledMethod1 refersToLiteral: #add)
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsGlobalVariable [

	self assert: (self compiledMethod1
				refersToLiteral: (self class environment associationAt: #Array))
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInPragmaArguments [

	self assert: (self compiledMethod1 refersToLiteral: 'bar').
	self assert: (self compiledMethod1 refersToLiteral: 123)
]

{ #category : 'tests - literals' }
CompiledCodeTest >> testRefersToLiteralsReturnsTrueWhenLiteralIsInPragmaSelector [

	self assert: (self compiledMethod1
				refersToLiteral: #pragma1:foo:)
]

{ #category : 'tests - flag' }
CompiledCodeTest >> testSetFlag [
	| method |
	method := self class >> #methodReturnOne.
	
	self deny: method flag.
	method setFlag.
	self assert: method flag.
	"we can set it twice"
	method setFlag.
	self assert: method flag.
	"we can execute the method"
	self assert: self methodReturnOne equals: 1.
	method clearFlag.
	self deny: method flag.
]

{ #category : 'tests - debugging support' }
CompiledCodeTest >> testStepIntoQuickMethods [
	|ctx compiledBlock|

	self deny: self compiledMethod1 stepIntoQuickMethods.
	ctx := [ 1 + 1 ] asContext.
	compiledBlock := ctx method.
	compiledBlock stepIntoQuickMethods: true.
	self assert: compiledBlock method stepIntoQuickMethods equals: compiledBlock stepIntoQuickMethods
]
