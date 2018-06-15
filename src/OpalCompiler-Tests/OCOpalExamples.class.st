"
I am the central place for example code used in tests
"
Class {
	#name : #OCOpalExamples,
	#superclass : #Object,
	#instVars : [
		'iVar',
		'collection'
	],
	#category : #'OpalCompiler-Tests-AST'
}

{ #category : #compiler }
OCOpalExamples class >> compilerClass [
	^OpalCompiler
]

{ #category : #examples }
OCOpalExamples >> doubleRemoteAnidatedBlocks [
	| val last |	
	val := 0. 
	1 to: 10 do: [ :i |
		[ :continue |
			i = 4 ifTrue: [continue value].
			val := val + 1.
			last := i
		] valueWithExit.
	].
	self assert: val = 9.
	self assert: last = 10.


]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleAndOr [
	| t1 t2 |
	t1 := true.
	t2 := false.
	t1 and: [ t2. '1' logCr ]. "effect"   
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleAndOr2 [
	| t1 t2 |
	t1 := true.
	t2 := false.  
	t2 or: [ t1. '2' logCr ]. "effect" 
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleAndOr3 [
	<sampleInstance>
	| t1 t2 |
	t1 := true.
	t2 := false.  
	^ t1 and: [ t2 ] "value" 
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleAndOr4 [
	<sampleInstance>
	| t1 t2 |
	t1 := true.
	t2 := false.  
	^ t2 or: [ t1 ] "value"  
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleAndOr5 [
	<sampleInstance>
	| t1 t2 |
	t1 := true.
	t2 := false.  
	^ t2 or: [ t1 and: [ t2 ] ]
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleAndOr6 [
	<sampleInstance>
	| t1 t2 |
	t1 := true.
	t2 := false.  
	^ t1 and: [ t2 or: [ t1 ] ]
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleBlockArgument [
	<sampleInstance>
	| block block1 block2 |
	block := [ :arg | | temp | temp := arg. [ temp ] ].
	block1 := block value: 1.
	block2 := block value: 2.
	self assert: block1 value = 1.
	self assert: block2 value = 2.
	^block1 value + block2 value.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleBlockExternal [
	<sampleInstance>
	| t |
	t := 1.
	^[t] value.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleBlockExternal2 [
	<sampleInstance>
	| t1 t2 |
	t1 :=  t2 := 1.
	^[t1 + t2] value.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleBlockExternalArg [
	<sampleInstance>
	| t  |
	t := 1.
	^[:a | t + a] value: 1.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleBlockExternalNested [
	<sampleInstance>
	| t s |
	t := s := 1.
	^[[s] value +   t   ] value.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleBlockInternal [
	<sampleInstance>
	
	^[ | t | t := 1. t] value
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleBlockMethodArgument: arg [
	<sampleInstance>
	|  block |
	block := [ :blockarg | blockarg + arg].
	self assert: ((block value: 2) = (arg + 2)).
	^block value: 2
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleBlockNested [
	<sampleInstance>
	
	^[ [1] value] value
]

{ #category : #'examples-pragmas' }
OCOpalExamples >> exampleDoublePrimitive [
	<primitive: 1>
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleEffectValues [
	#getMe.
	#( got that ? ).
	Class.
	
]

{ #category : #'examples-simple' }
OCOpalExamples >> exampleEmptyMethod [
]

{ #category : #'examples-variables' }
OCOpalExamples >> exampleForBlockArgument [
	<sampleInstance>
	|b|
	b:= [ :blockArg | 1 ].
	^ b
]

{ #category : #'examples-variables' }
OCOpalExamples >> exampleForInlinedBlockArgument [
	<sampleInstance>
	
	1 ifNotNil:[ :value | ^ value ].

]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleIfFalse [
	<sampleInstance>
	true ifFalse: [^1].
	^2
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleIfFalseIfTrue [
	true ifFalse: [^1] ifTrue: [^2].
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleIfNotNilArg [
	<sampleInstance>
	^1 even ifNotNil: [ :arg | arg not ]
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleIfNotNilReturnNil [
	<sampleInstance>
	^nil ifNotNil: [ :arg | arg not ]
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleIfTrue [
	<sampleInstance>
	 1  < 2 ifTrue: [^'result']. 
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleIfTrueAssign [
	<sampleInstance>
	| a |
	a := 1 <2 ifTrue: [1] ifFalse: [2].
	^a 
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleIfTrueIfFalse [
	<sampleInstance>
	 1 <2 ifTrue: [^'result'] ifFalse: [2].
	
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleInlineBlockCollectionLR3 [
	<sampleInstance>
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
	^ (col collect: [ :each | each value ]) asArray "= (2 to: 12) asArray"

]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleMethodTempInNestedBlock [
	<sampleInstance>
	| temp block |
	temp := 0.
	block := [ [ temp ] ].
	temp := 1.
	block := block value.
	temp := 2.
	"self assert: block value = 2."
	^block value.
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleMethodWithOptimizedBlocksA [
	<sampleInstance>
	| s c |
	s := self isNil
			ifTrue: [| a | a := 'isNil'. a]
			ifFalse: [| a | a := 'notNil'. a].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c. "notNil"
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleNestedBlockScoping [
	<sampleInstance>

	| b c z |
	b := [:a | 
			z := 2.
			z + a].
	c := [:a |
			z + a].
	^ (b value: 2) + (c value: 1)
]

{ #category : #'examples-simple' }
OCOpalExamples >> exampleNewArray [
	<sampleInstance>

	| temp |
	
	temp := Array new: 3.
	^temp
]

{ #category : #'examples-pragmas' }
OCOpalExamples >> examplePrimitiveErrorCode [
	<sampleInstance>
	
	<primitive: 60 error: code >
	
	^ code


]

{ #category : #'examples-pragmas' }
OCOpalExamples >> examplePrimitiveErrorCodeModule [
	<sampleInstance>
	"Primitive. Attempt to load a module of the given name.
	Fail if module cannot be found, or cannot be loaded,
	or failed to initialize"

	<primitive: 'primLoadModule'  module: '' error: #errorCode >
	^ self primitiveFailed
]

{ #category : #'examples-pragmas' }
OCOpalExamples >> examplePrimitiveErrorModule [
	<sampleInstance>
	
	<primitive: 'primFunction' error: errorCode module: 'primModule'>
	^ errorCode
]

{ #category : #'examples-pragmas' }
OCOpalExamples >> examplePrimitiveModuleError [
	<sampleInstance>
	<primitive: 'primFunction'  module: 'primModule' error: errorCode >
	^ errorCode
]

{ #category : #'examples-misc' }
OCOpalExamples >> examplePushArray [
	<sampleInstance>
	| t |

	{1 .t:=1}.
	^t
]

{ #category : #'examples-misc' }
OCOpalExamples >> examplePushBigArray [
	<sampleInstance>
	"This array should have a size more than 127 elements"
	{ 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 .  255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 . 255 }.
	^ 1
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleRepeatEffect [
	<sampleInstance>
	<compilerOptions: #(#+ #optionInlineRepeat)>
	| i |
	i := 1.
	[  
	i := i + 1. 
	i = 10
		ifTrue: [ ^ true ] ] repeat
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleRepeatValue [
	<sampleInstance>
	<compilerOptions: #(#+ #optionInlineRepeat)>
	| i |
	i := 1.
	^ [ 
	i := i + 1.
	i = 10
		ifTrue: [ ^ true ] ] repeat 
]

{ #category : #'examples-simple' }
OCOpalExamples >> exampleReturn1 [
	<sampleInstance>
	^1
]

{ #category : #'examples-simple' }
OCOpalExamples >> exampleReturn1plus2 [
	<sampleInstance>
	^1+2
]

{ #category : #'examples-simple' }
OCOpalExamples >> exampleReturn42 [
	<sampleInstance>
	^42
]

{ #category : #'examples-variables' }
OCOpalExamples >> exampleSelf [
	<sampleInstance>
	| t |
	t := self.
	^self.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSelfReceiver [
	<sampleInstance>
	self foo.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlock [
	<sampleInstance>
	^[1].
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockArgument1 [
	<sampleInstance>
	^[:a | a ] value: 1.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockArgument2 [
	<sampleInstance>
	^[:a :b | a + b ] value: 1 value: 1.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockArgument3 [
	<sampleInstance>
	^[:a :b :c | a + b + c ] value: 1 value: 1 value: 1.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockArgument4 [
	<sampleInstance>
	^[:a :b :c :d | a + b + c  + d] value: 1 value: 1 value: 1 value: 1. 
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockArgument5 [
	<sampleInstance>
	^[:a :b :c :d :e| a + b + c  + d + e] valueWithArguments: #(1 1 1 1 1). 
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockEmpty [
	<sampleInstance>
	^[] value.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockLocal [
	<sampleInstance>
	^[ :each | | t |  t:= each. t   ] value: 5.
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleSimpleBlockLocalIf [
	<sampleInstance>
	^true ifTrue: [ | hallo |  hallo := 1 . hallo].
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockLocalIfNested [
	<sampleInstance>
	^true ifTrue: [| hallo |  [  hallo := 1 . hallo] value] .
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleSimpleBlockLocalWhile [
	<sampleInstance>
	|a|
	a := true.
	^[: b | [a] whileTrue: [ | hallo |  a := false. hallo := 1 . hallo]]value: 1.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockNested [
	<sampleInstance>
	| a match dict | 

	a  := #(a b c d).
	dict := Dictionary new.


	a doWithIndex: [:each :index |
		(match := a indexOf: each) > 0 ifTrue:
			[dict at: index put: (a at: match)]].

	^ dict.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockReturn [
	<sampleInstance>
	[^1] value.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSimpleBlockiVar [
	<sampleInstance>
	^[iVar] value.
]

{ #category : #'examples-variables' }
OCOpalExamples >> exampleSuper [
	<sampleInstance>
	| t |
	t := super.
	^super.
]

{ #category : #'examples-blocks' }
OCOpalExamples >> exampleSuperReceiver [
	<sampleInstance>
	super foo.
]

{ #category : #'examples-variables' }
OCOpalExamples >> exampleThisContext [
	<sampleInstance>
	| t |
	t := thisContext.
	^thisContext.
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleTimesRepeatEffect [
	<sampleInstance>
	| foo |
	foo := 1.
	5 timesRepeat: [ foo := foo + 3 ] . 
	^ foo
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleTimesRepeatValue [
	<sampleInstance>
	^ 5 timesRepeat: [ 1 + 2 ]   
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleToDoArgument [
	<sampleInstance>
	collection := OrderedCollection new.
	1 to: 5 do: [ :index |
		collection add:  [index]  ].
	 ^collection collect: #value. 
	"#(1 2 3 4 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleToDoArgumentLimitIsExpression [
	<sampleInstance>
	| count sum |
	count := 10.
	sum := 0.
	1 to: count - 1 do: [ :each | sum := sum + each].
	^sum
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleToDoArgumentNotInlined [
	<sampleInstance>
	| block |
	block := [ :index |
		collection add: [ index ] ].
	1 to: 5 do: block.
	^collection "#(1 2 3 4 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleToDoInsideTemp [
	<sampleInstance>
	1 to: 5 do: [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	^collection "#(1 2 3 4 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleToDoInsideTempNotInlined [
	<sampleInstance>
	| block |
	block := [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	^collection "#(1 2 3 4 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleToDoOutsideTemp [
	<sampleInstance>
	| temp |
	1 to: 5 do: [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	^collection "#(5 5 5 5 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleToDoOutsideTempNotInlined [
	<sampleInstance>
	| block temp |
	block := [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	^collection "#(5 5 5 5 5)"
]

{ #category : #'examples-misc' }
OCOpalExamples >> exampleToDoValue [
	<sampleInstance>

	^ 1 to: 2 do: [:each | each]
	

]

{ #category : #'examples-misc' }
OCOpalExamples >> exampleToDoValueLimitExpression [
	<sampleInstance>

	^ 2 to: 3+4 do: [:each | each]
	

]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleWhileModificationAfterNotInlined [
	<sampleInstance>
	| index block |
	index := 0.
	block := [ 
		collection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	^collection "#(5 5 5 5 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleWhileModificationBefore [
	<sampleInstance>
	| index |
	collection := OrderedCollection new.
	index := 0.
	[ index < 5 ] whileTrue: [ 
		index := index + 1.
		collection add: [ index ] ].
	 ^collection collect: #value.
	"#(5 5 5 5 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleWhileModificationBeforeNotInlined [
	<sampleInstance>
	| index block |
	index := 0.
	block := [ 
		index := index + 1.
		collection add: [ index ] ].
	[ index < 5 ] whileTrue: block.
	^collection.
	"#(5 5 5 5 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleWhileNoModification [
	"tempvars aren't modified in the loop, therefore they don't need to be in a indirection vector.
	  number of temps = declared temps "

	| a b c |
	a := 1.
	b := 2.
	c := 3.
	[ false ]
		whileTrue: [ 
			[ a class.
			  b class.
			  c class ]	"<- block in optimized loop" ]
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleWhileWithTemp [
	<sampleInstance>
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	^collection "#(1 2 3 4 5)"
]

{ #category : #'examples-blocks-optimized' }
OCOpalExamples >> exampleWhileWithTempNotInlined [
	<sampleInstance>
	| index block |
	index := 0.
	block := [
		| temp |
		temp := index := index + 1.
		collection add: [ temp ] ].
	[ index < 5 ] whileTrue: block.
	^collection "#(1 2 3 4 5)"
]

{ #category : #'examples-variables' }
OCOpalExamples >> exampleWithArgument: anArg [
	<sampleInstance>
	^ anArg
]

{ #category : #'examples-variables' }
OCOpalExamples >> exampleiVar [
	<sampleInstance>
	iVar := 1.
	^iVar.
]

{ #category : #initialization }
OCOpalExamples >> initialize [
	collection := OrderedCollection new
]

{ #category : #examples }
OCOpalExamples >> nestedBlocksRemoteInBlockCase1 [
	| block |
	block := [ | a b |
		[ a := b ].
		[ b := a ] ]
]

{ #category : #examples }
OCOpalExamples >> nestedBlocksRemoteInBlockCase2 [
	| block |
	block := [ 
		[ |a| [ a := 1] ].
		[ |b| [ b := 2] ] ]
]

{ #category : #examples }
OCOpalExamples >> nestedBlocksRemoteInBlockCase3 [
	| block |
	block := [ 
		[ |a| block := [ a := 1] ].
		[ |b| block := [ b := 2] ] ].
]

{ #category : #examples }
OCOpalExamples >> noRemoteBlockArgument [
	| block block1 block2 |
	block := [ :arg | | temp | temp := arg. [ temp ] ].
	block1 := block value: 1.
	block2 := block value: 2.


]

{ #category : #examples }
OCOpalExamples >> noRemoteBlockReturn [
	(self announcements at: self index ifAbsent: [ ^ self ]) 
		open.


]

{ #category : #examples }
OCOpalExamples >> noRemoteBlockTemp [
	| block block1 block2 |
	block := [ :arg | [ arg ] ].
	block1 := block value: 1.
	block2 := block value: 2.



]

{ #category : #examples }
OCOpalExamples >> noRemoteMethodTemp [
	| block1 block2 |
	block1 := self methodArgument: 1.
	block2 := self methodArgument: 2.


]

{ #category : #examples }
OCOpalExamples >> noRemoteReadInBlock [
	| a |
	a := 1.
	[ a ]


]

{ #category : #examples }
OCOpalExamples >> noRemoteReadNestedBlocks [   
	 [ |a| a := 1.
       [ a ]]

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockReadInBlock [
	| t1 |
	[ t1 < 5 ] whileTrue: [ [ t1 ] ].

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockWriteInBlock [
	| t1 |
	[ t1 < 5 ] whileTrue: [ t1 := 1 ].

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockWriteInNestedBlock [
	| t1 |
	^[ [ t1 < 5 ] whileTrue: [ t1 := 1 ] ]

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockWriteInNestedBlockCase2 [
	| t1 |
	(true) ifTrue: [ [ t1 < 5 ] whileTrue: [ t1 := 1 ] ]

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockWriteInNestedBlockCase3 [
	| t1 |
	[(true) ifTrue: [ [ t1 < 5 ] whileTrue: [ t1 := 1 ] ] ]

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockWriteInNestedBlockCase4 [
	| t1 |
	[(true) whileTrue: [ [ t1 < 5 ] whileTrue: [ t1 := 1 ] ] ]

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockWriteReadInBlock [
	| t1 |
	[ t1 < 5 ] whileTrue: [ t1 := 1. [ t1 ] ].

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockWrittenAfterClosedOverCase1 [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		[ temp ].
		temp := index := index + 1].
	^index

]

{ #category : #examples }
OCOpalExamples >> optimizedBlockWrittenAfterClosedOverCase2 [
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		temp := index := index + 1.
		 [ temp ] ].

]

{ #category : #examples }
OCOpalExamples >> optimizedBlocksAndSameNameTemps [
	| s c |
	s := self isNil
			ifTrue: [| a | a := 1. a]
			ifFalse: [| a | a := 2. a].
	c := String new: s size.
	1 to: s size do:
		[:i| c at: i put: (s at: i)].
	^c
]

{ #category : #examples }
OCOpalExamples >> singleRemoteDifferentBlocksSameArgumentName [
	| b c z |
	b := [:a | 
		z := 2.
		z + a].
	c := [:a |
		z + a].
	^ (b value: 2) + (c value: 1)


]

{ #category : #examples }
OCOpalExamples >> singleRemoteMethodArgument [
	| temp block |
	temp := 0.
	block := [ [ temp ] ].
	temp := 1.
	block := block value.
	temp := 2.

]

{ #category : #examples }
OCOpalExamples >> singleRemoteReadNestedBlocks [   
	|a| 
	[  a := 1.
       [ a ]]

]

{ #category : #examples }
OCOpalExamples >> singleRemoteTempVar [
	| index block theCollection |
	index := 0.
	block := [ 
		theCollection add: [ index ].
		index := index + 1 ].
	[ index < 5 ] whileTrue: block.
	self assertValues: #(5 5 5 5 5)
]

{ #category : #examples }
OCOpalExamples >> singleRemoteTempVarWrittenAfterClosedOver [
	| index block |
	index := 0.
	block := [ index ].
	index := index + 1.

]

{ #category : #examples }
OCOpalExamples >> writtenAfterClosedOver [
	| a |      
	[ a ].
	a := 1
]
