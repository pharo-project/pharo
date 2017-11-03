"
I scan compiled method to detect loops. I answer an array or the size of the compiled method I scanned, which holds a FBDLoop at the pc where a loop starts and nil for any other pc. The FBDLoop describes the loop.

instructionStream <InstructionStream> allows me to decode the method's bytecode
branchTargets <Array of (Smi | nil)> at any pc where a conditional jump goes, I put an annotation in this array. This is used to know if a loop is conditional or unconditional.
loops <Array of (FBDLoop | nil)> array answered by the scanner. it holds a FBDLoop at the pc where a loop starts and nil for any other pc.
currentPC <Smi> by opposition to self pc which holds the pc just *after* the instruction being decoded, currentPC holds the pc of the instruction being decoded. This is useful as you can't read the bytecode backward due to multiple bytecodes instructions.

"
Class {
	#name : #FBDLoopScanner,
	#superclass : #InstructionClient,
	#instVars : [
		'instructionStream',
		'branchTargets',
		'loops',
		'currentPC'
	],
	#category : #Flashback-Decompiler
}

{ #category : #'public api' }
FBDLoopScanner class >> scan: method [
	^ self new scan: method
]

{ #category : #'instruction decoding' }
FBDLoopScanner >> interpretMethod: method [
	[ instructionStream pc > method endPC ]
		whileFalse: [ 
			currentPC := self pc.
			instructionStream interpretNextInstructionFor: self ]
]

{ #category : #'instruction decoding' }
FBDLoopScanner >> jump: offset [
	| loop backjumpTarget |
	offset < 0 ifFalse: [ ^ self ].
	loop := (branchTargets at: self pc)
		ifNotNil: [ :exitPC | FBDConditionalLoop new exitCondition: exitPC; yourself ]
		ifNil: [ FBDLoop new ].
	backjumpTarget := self pc + offset.
	loop backjump: currentPC.
	(loops at: backjumpTarget) 
		ifNil: [ loops at: backjumpTarget put: (OrderedCollection with: loop) ]
		ifNotNil: [ :col | col add: loop ].
]

{ #category : #'instruction decoding' }
FBDLoopScanner >> jump: offset if: bool [
	branchTargets at: self pc + offset put: currentPC
]

{ #category : #accessing }
FBDLoopScanner >> pc [
	^ instructionStream pc
]

{ #category : #'public api' }
FBDLoopScanner >> scan: method [
	loops := Array new: method endPC.
	branchTargets := Array new: method endPC.
	instructionStream := InstructionStream on: method.
	self interpretMethod: method.
	^ loops
]
