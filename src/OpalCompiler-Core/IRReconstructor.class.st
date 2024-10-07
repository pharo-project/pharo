"
I am a specialized IRBuilder for the decompiler
"
Class {
	#name : 'IRReconstructor',
	#superclass : 'IRBuilder',
	#instVars : [
		'temps',
		'remoteTemps',
		'closureCopiedValues'
	],
	#category : 'OpalCompiler-Core-Bytecode',
	#package : 'OpalCompiler-Core',
	#tag : 'Bytecode'
}

{ #category : 'instructions' }
IRReconstructor >> blockReturnTop [

	self fixPushNilsForTemps.
	^ super blockReturnTop
]

{ #category : 'initialize' }
IRReconstructor >> createTempVectorNamed: arg1 withVars: arg2 [

	sourceMapByteIndex := sourceMapByteIndex - 2.
	self add: (IRInstruction createTempVectorNamed: arg1 withVars: arg2)
]

{ #category : 'accessing' }
IRReconstructor >> currentSequence [

	^ currentSequence
]

{ #category : 'accessing' }
IRReconstructor >> currentSequence: arg1 [

	currentSequence := arg1
]

{ #category : 'removing' }
IRReconstructor >> fixPushNilsForTemps [

	| tmp1 |
	tmp1 := self currentScope blockSequence sequence.
	self currentScope definedTemps do: [ :arg1 | tmp1 removeFirst ]
]

{ #category : 'initialization' }
IRReconstructor >> initialize [

	temps := Dictionary new.
	remoteTemps := Dictionary new.
	closureCopiedValues := Dictionary new.
	sourceMapByteIndex := 0.
	super initialize
]

{ #category : 'testing' }
IRReconstructor >> isLastClosureInstruction [

	| tmp1 |
	tmp1 := jumpAheadStacks
		        at: sourceMapByteIndex + 1
		        ifAbsent: [ ^ false ].
	^ tmp1 anySatisfy: [ :arg1 | arg1 = self currentScope ]
]

{ #category : 'instructions' }
IRReconstructor >> pushFullClosureCompiledBlock: arg1 copiedValues: arg2 [

	| tmp1 |
	tmp1 := super pushFullClosureCompiledBlock: arg1 copiedValues: arg2.
	tmp1 copiedValues withIndexDo: [ :arg3 :arg4 |
		self
			rememberReference: tmp1 -> arg4
			to: arg3
			in: closureCopiedValues ]
]

{ #category : 'instructions' }
IRReconstructor >> pushRemoteTemp: arg1 inVector: arg2 [

	| tmp1 |
	tmp1 := super pushRemoteTemp: arg1 inVector: arg2.
	self rememberReference: tmp1 to: arg2 in: remoteTemps
]

{ #category : 'instructions' }
IRReconstructor >> pushTemp: arg1 [

	| tmp1 |
	tmp1 := super pushTemp: arg1.
	self rememberReference: tmp1 to: arg1 in: temps
]

{ #category : 'remapping' }
IRReconstructor >> remapTemp: arg1 toRemote: arg2 [

	(temps removeKey: arg1 ifAbsent: [ #(  ) ]) do: [ :arg3 |
		arg3 name: arg2.
		self rememberReference: arg3 to: arg2 in: temps ].
	(remoteTemps removeKey: arg1 ifAbsent: [ #(  ) ]) do: [ :arg4 |
		arg4 tempVectorName: arg2.
		self rememberReference: arg4 to: arg2 in: remoteTemps ].
	(closureCopiedValues removeKey: arg1 ifAbsent: [ #(  ) ]) do: [ :arg5 |
		| tmp1 tmp2 |
		tmp1 := arg5 key.
		tmp2 := arg5 value.
		tmp1 remapCopiedValueAt: tmp2 oldOne: arg1 newOne: arg2.
		self rememberReference: arg5 to: arg2 in: closureCopiedValues ]
]

{ #category : 'remapping' }
IRReconstructor >> rememberReference: arg1 to: arg2 in: arg3 [

	(arg3 at: arg2 ifAbsentPut: [ OrderedCollection new ]) add: arg1
]

{ #category : 'removing' }
IRReconstructor >> removeLast: arg1 [

	sourceMapByteIndex := sourceMapByteIndex - arg1.
	^ (currentSequence removeLast: arg1) collect: [ :arg2 |
		  arg2 isTemp ifFalse: [
			  self error: 'Should only remove temp accesses!' ].
		  arg2 name ]
]

{ #category : 'instructions' }
IRReconstructor >> storeRemoteTemp: arg1 inVector: arg2 [

	| tmp1 |
	tmp1 := super storeRemoteTemp: arg1 inVector: arg2.
	self rememberReference: tmp1 to: arg2 in: remoteTemps
]

{ #category : 'instructions' }
IRReconstructor >> storeTemp: arg1 [

	| tmp1 |
	tmp1 := super storeTemp: arg1.
	self rememberReference: tmp1 to: arg1 in: temps
]
