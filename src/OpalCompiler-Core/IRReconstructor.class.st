"
I am a specialized IRBuilder for the decompiler
"
Class {
	#name : #IRReconstructor,
	#superclass : #IRBuilder,
	#instVars : [
		'temps',
		'remoteTemps',
		'closureCopiedValues'
	],
	#category : #OpalCompiler-Core-Bytecode
}

{ #category : #instructions }
IRReconstructor >> blockReturnTop [
	self fixPushNilsForTemps.
	^ super blockReturnTop
]

{ #category : #initialize }
IRReconstructor >> createTempVectorNamed: name withVars: anArray [
	" Don't add the temp yet, we only know it's index at the end of the block or method "
	"self addVectorTemps: anArray"
	" Update the byte index to point before the pushing of the new vector "
	sourceMapByteIndex := sourceMapByteIndex - 2.
	self add: (IRInstruction createTempVectorNamed: name withVars: anArray)
]

{ #category : #accessors }
IRReconstructor >> currentSequence [
	^currentSequence 
]

{ #category : #accessors }
IRReconstructor >> currentSequence: aSeq [
	currentSequence := aSeq
]

{ #category : #removing }
IRReconstructor >> fixPushNilsForTemps [
	" There are pushConstant: nil in the beginning of the blocksequence for all of the defined temps.
	  We got these pushConstant: nil in. Now our closure will generate them again, meaning we will double
	  the number of pushConstant: nil in the final block. To avoid this, we strip the ones we got in from the IR.  "

	| blocksequence |
	blocksequence := self currentScope blockSequence sequence.
	self currentScope definedTemps do: [ :temp | blocksequence removeFirst ]. 
	
]

{ #category : #initialization }
IRReconstructor >> initialize [
	temps := Dictionary new.
	remoteTemps := Dictionary new.
	closureCopiedValues := Dictionary new.
	sourceMapByteIndex := 0.
	super initialize.
]

{ #category : #testing }
IRReconstructor >> isLastClosureInstruction [
	| nextJumps |
	nextJumps := jumpAheadStacks at: sourceMapByteIndex + 1 ifAbsent: [ ^ false ].
	^ nextJumps anySatisfy: [ :anOrigin | anOrigin = self currentScope ]
]

{ #category : #instructions }
IRReconstructor >> pushClosureCopyCopiedValues: copiedValuesNames args: args jumpTo: aJumpLabel [
	| anInstruction |
	anInstruction := super pushClosureCopyCopiedValues: copiedValuesNames args: args jumpTo: aJumpLabel.
	"remove pushed nils of local temps"
	anInstruction copiedValues withIndexDo: [ :aValue :index |
		self rememberReference: anInstruction -> index to: aValue in: closureCopiedValues ]
]

{ #category : #instructions }
IRReconstructor >> pushRemoteTemp: name inVector: nameOfVector [
	| anInstruction |
	anInstruction := super pushRemoteTemp: name inVector: nameOfVector.
	self rememberReference: anInstruction to: nameOfVector in: remoteTemps.
]

{ #category : #instructions }
IRReconstructor >> pushTemp: aSelector [
	| anInstruction |
	anInstruction := super pushTemp: aSelector.
	self rememberReference: anInstruction to: aSelector in: temps.
]

{ #category : #remapping }
IRReconstructor >> remapTemp: aTemp toRemote: aRemote [
	(temps removeKey: aTemp ifAbsent: [ #() ])
		do: [ :tempAccess |
			tempAccess name: aRemote.
			self rememberReference: tempAccess to: aRemote in: temps ].
		
	(remoteTemps removeKey: aTemp ifAbsent: [ #() ])
		do: [ :tempAccess |
			tempAccess tempVectorName: aRemote.
			self rememberReference: tempAccess to: aRemote in: remoteTemps. ].

	(closureCopiedValues removeKey: aTemp ifAbsent: [ #() ])
		do: [ :aClosureAndIndex | |closure index|
			closure := aClosureAndIndex key.
			index := aClosureAndIndex value.
			closure copiedValues at: index put: aRemote.
			closure tempMap at: aRemote put: (closure tempMap removeKey: aTemp).
			self rememberReference: aClosureAndIndex to: aRemote in: closureCopiedValues. ].
]

{ #category : #remapping }
IRReconstructor >> rememberReference: anInstruction to: name in: dictionary [
	(dictionary at: name ifAbsentPut: [ OrderedCollection new ])
		add: anInstruction
]

{ #category : #removing }
IRReconstructor >> removeLast: n [
	" Make the address of the instruction be the address of the first removed instruction. "
	sourceMapByteIndex := sourceMapByteIndex - n.
	^ (currentSequence removeLast: n) collect: [ :node |
		node isTemp ifFalse: [ self error: 'Should only remove temp accesses!' ].
		node name ]
]

{ #category : #instructions }
IRReconstructor >> storeRemoteTemp: name inVector: nameOfVector [
	| anInstruction |
	anInstruction := super storeRemoteTemp: name inVector: nameOfVector.
	self rememberReference: anInstruction to: nameOfVector in: remoteTemps.
]

{ #category : #instructions }
IRReconstructor >> storeTemp: aSelector [
	| anInstruction |
	anInstruction := super storeTemp: aSelector.
	self rememberReference: anInstruction to: aSelector in: temps.
]
