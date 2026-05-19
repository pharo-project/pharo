Class {
	#name : 'OCClosureCompilerTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-FromOld',
	#package : 'OpalCompiler-Tests',
	#tag : 'FromOld'
}

{ #category : 'accessing' }
OCClosureCompilerTest class >> compilerClass [
	^OpalCompiler
]

{ #category : 'code examples' }
OCClosureCompilerTest class >> methodWithCopiedAndAssignedTemps [
	| blk "0w" a "0w" b "0w" c "0w" t "0w" r1 "0w" r2 "0w" |
	a := 1. "1w"
	b := 2. "1w"
	c := 4. "1w"
	t := 0. "1w"
	blk "5w" := ["2" t  "3w" := t "3r" + a "3r" + b "3r" + c "3r" ] "4".
	r1 "5w" := blk "5r" value.
	b "5w" := -100.
	r2 "5w" := blk "5r" value.
	^r1 "5r" -> r2 "5r" -> t "5r"

	"a: main(read(),write(0,1)), block(read(3),write()) => copy; no writes follow read
	 b: main(read(),write(0,1,5)), block(read(3),write()) => remote; write follows contained read
	 blk: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 c: main(read(),write(0,1)), block(read(3),write()) => copy; no writes follow read
	 r1: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 r2: main(read(5),write(0,5)), block(read(),write()) => no copy in blocks < 5
	 t: main(read(5),write(0,1)), block(read(3),write(3)) => remote; read follows contained write"


	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedAndAssignedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"
]

{ #category : 'code examples' }
OCClosureCompilerTest class >> methodWithCopiedAndPostClosedOverAssignedTemps [
	| blk a b c r1 r2 |
	a := 1.
	b := 2.
	c := 4.
	blk := [a + b + c].
	r1 := blk value.
	b := nil.
	r2 := blk value.
	r1 -> r2

	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedAndPostClosedOverAssignedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"
]

{ #category : 'code examples' }
OCClosureCompilerTest class >> methodWithCopiedTemps [
	| a b c r |
	a := 1.
	b := 2.
	c := 4.
	r := [a + b + c] value.
	b := nil.
	r

	"Parser new
		parse: (self class sourceCodeAt: #methodWithCopiedTemps)
		class: self class"

	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"
]

{ #category : 'code examples' }
OCClosureCompilerTest class >> methodWithOptimizedBlocks [
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| b | b := 'notNil'. b].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c

	"Parser new
		parse: (self class sourceCodeAt: #methodWithOptimizedBlocks)
		class: self class"
]

{ #category : 'code examples' }
OCClosureCompilerTest class >> methodWithOptimizedBlocksA [
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| a | a := 'notNil'. a].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c

	"Parser new
		parse: (self class sourceCodeAt: #methodWithOptimizedBlocksA)
		class: self class"
]

{ #category : 'code examples' }
OCClosureCompilerTest class >> methodWithVariousTemps [
	| classes total totalLength |
	classes := self withAllSuperclasses.
	total := totalLength := 0.
	classes do: [:class| | className |
		className := class name.
		total := total + 1.
		totalLength := totalLength + className size].
	^total -> totalLength

	"Parser new
		parse: (self class sourceCodeAt: #methodWithVariousTemps)
		class: self class"
]

{ #category : 'tests-block collection' }
OCClosureCompilerTest >> testInlineBlockCollectionEM1 [

	| a1 b1 i1 a2 b2 i2 we wb |
	b1 := OrderedCollection new.
	i1 := 1.
	[ a1 := i1.
	i1 <= 3 ]
		whileTrue: [ b1 add: [ a1 ].
			i1 := i1 + 1 ].
	b1 := b1 asArray collect: [ :b | b value ].
	b2 := OrderedCollection new.
	i2 := 1.
	we := [ a2 := i2.
		i2 <= 3 ].
	wb := [ b2 add: [ a2 ].
	i2 := i2 + 1 ].
	we whileTrue: wb.	"defeat optimization"
	b2 := b2 asArray collect: [ :b | b value ].
	self assert: b1 equals: b2
]

{ #category : 'tests-block collection' }
OCClosureCompilerTest >> testInlineBlockCollectionLR1 [

	| col | 
	col := OrderedCollection new.
	1 to: 11 do: [ :each | col add: [ each ] ].
	self 
		assert: (col collect: [ :each | each value ]) asArray 
		equals: (1 to: 11) asArray
]

{ #category : 'tests-block collection' }
OCClosureCompilerTest >> testInlineBlockCollectionLR2 [

	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | #(1) do: [ :ignored | col add: [ each ] ] ].
	self 
		assert: (col collect: [ :each | each value ]) asArray 
		equals: (1 to: 11) asArray
]

{ #category : 'tests-block collection' }
OCClosureCompilerTest >> testInlineBlockCollectionLR3 [

	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each |
		| i |
		i := each.
		col add: [ i ].
		i := i + 1 ].
	self 
		assert: (col collect: [ :each | each value ]) asArray 
		equals: (2 to: 12) asArray
]

{ #category : 'tests-block collection' }
OCClosureCompilerTest >> testInlineBlockCollectionSD1 [

	| a1 b1 a2 b2 |
	b1 := OrderedCollection new.
	1 to: 3 do: [ :i |
		a1 := i.
		b1 add: [ a1 ] ].
	b1 := b1 asArray collect: [ :b | b value ].
	b2 := OrderedCollection new.
	1 to: 3 do:
		[ :i |
		a2 := i.
		b2 add: [ a2 ] ] yourself.	"defeat optimization"
	b2 := b2 asArray collect: [ :b | b value ].
	self 
		assert: b1 
		equals: b2
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableAssignedInside [

	| a |
	a := 23.
	1 to: 17 do: [ :i | | a |
		a := 7 ].
	self assert: a equals: 23
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableInnerBlocks [

	| n |
	n := 1.
	n > 0 ifTrue: [ | n | 
		n := 2.
		n > 0 ifTrue: [ | n | 
			n := 3. ]. 
		self assert: n equals: 2 ].
	self assert: n equals: 1
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableParameter [
	| a |
	a := 23.
	1 to: 17 do: [ :a | ].
	self assert: a equals: 23
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableParameterAndVariable [

	| result |
	result := 1 ifNil: [ | res | ] ifNotNil: [ :res | res ].
	self assert: result equals: 1 
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableParameterUsedInInnerBlock [
	| a result |
	a := 23.
	1 to: 17 do: [ :i |
		[result := i] value ].
	self assert: a equals: 23.
	self assert: result equals: 17  
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableTemporary [
	| a |
	a := 23.
	1 to: 17 do: [ :i | | a | ].
	self assert: a equals: 23 
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableTwoBlocksParameter [

	| first second |
	1 ifNotNil: [ :n | first := n ].
	2 ifNotNil: [ :n | second := n ].
	self assert: first equals: 1.
	self assert: second equals: 2 
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableTwoBlocksTemporary [
	| a |
	a := 23.
	1 to: 17 do: [ :i | | a | 
		self assert: a isNil ].
	1 to: 17 do: [ :i | | a | 
		a := 7.
		self assert: a equals: 7 ].
	self assert: a equals: 23 
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableUsedInBlock [
	| a result |
	a := 23.
	1 to: 17 do: [ :i | | a | 
		result := a ].
	self assert: a equals: 23.
	self assert: result isNil
	

]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableUsedInInnerBlock [
	| a result |
	a := 23.
	1 to: 17 do: [ :i | | a | 
		[result := a] value ].
	self assert: a equals: 23.
	self assert: result isNil 
]

{ #category : 'tests-block shadow' }
OCClosureCompilerTest >> testInlineBlockShadowVariableUsedInInnerInnerBlock [

	| a result |
	a := 23.
	1 to: 7 do: [ :a |
		7 to: 17 do: [ :j | 
			a.
			[ result := a ] value ] ].

	self assert: a equals: 23.
	self assert: result equals: 7
]

{ #category : 'tests' }
OCClosureCompilerTest >> testOptimizedBlockLocalNilling1 [
	"Whether a block is optimized or not a block-local temp
	 should be nil at the start of each evaluation of the block."

	1 to: 3 do: [:i| | j |
		self assert: j isNil.
		j := i + 1.
		self assert: j isNil not]
]

{ #category : 'tests' }
OCClosureCompilerTest >> testOptimizedBlockLocalNilling2 [
	"Whether a block is optimized or not a block-local temp
	 should be nil at the start of each evaluation of the block."

	1 to: 6 do: [:i| | j k |
		self assert: j isNil.
		self assert: k isNil.
		i even
			ifTrue: [j := i + 2]
			ifFalse: [k := i + 1].
		self assert: (j isNil or: [k isNil]).
		self assert: (j isNil not or: [k isNil not])]
]
