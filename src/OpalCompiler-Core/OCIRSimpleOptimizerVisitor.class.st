"
I perform some simple optimizations by changing IR nodes.
I transform:

- sequence of store, pop into popInto
- some returns into quick returns
"
Class {
	#name : 'OCIRSimpleOptimizerVisitor',
	#superclass : 'OCIRVisitor',
	#instVars : [
		'prevInstr',
		'storePopToFix',
		'retToFix'
	],
	#category : 'OpalCompiler-Core-IR-Manipulation',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Manipulation'
}

{ #category : 'private' }
OCIRSimpleOptimizerVisitor >> convertReturn: assoc forInstructionSequence: seq [
	| push ret |
	push := assoc key.
	ret := assoc value.
	seq replaceNode: push withNode: (push quickRetNode sourceNode: ret sourceNode).
	seq remove: ret
]

{ #category : 'private' }
OCIRSimpleOptimizerVisitor >> convertStorePop: assoc forinstructionSequence: seq [
	| store pop |
	store := assoc key.
	pop := assoc value.
	seq replaceNode: store withNode: (store popIntoNode sourceNode: store sourceNode).
	seq remove: pop
]

{ #category : 'initialization' }
OCIRSimpleOptimizerVisitor >> initialize [

	super initialize.

	storePopToFix := OrderedCollection new.
	retToFix := OrderedCollection new
]

{ #category : 'visiting' }
OCIRSimpleOptimizerVisitor >> visitInstruction: instr [
	self visitNode: instr.
	prevInstr := instr
]

{ #category : 'visiting' }
OCIRSimpleOptimizerVisitor >> visitPop: pop [
	prevInstr ifNil: [ ^ self ].
	prevInstr isStore ifFalse: [ ^ self ].
	"store then pop, however, cannot remove instr while iterating over the collection"
	storePopToFix add: prevInstr -> pop
]

{ #category : 'visiting' }
OCIRSimpleOptimizerVisitor >> visitReturn: ret [
	prevInstr ifNil: [ ^ self ].
	prevInstr canBeQuickReturn ifFalse: [ ^ self ].
	retToFix add: prevInstr -> ret
]

{ #category : 'visiting' }
OCIRSimpleOptimizerVisitor >> visitSequence: instructionSequence [
	prevInstr := nil.
	storePopToFix reset.
	retToFix reset.
	super visitSequence: instructionSequence.
	retToFix do: [ :each | self convertReturn: each forInstructionSequence: instructionSequence ].
	storePopToFix do: [ :each | self convertStorePop: each forinstructionSequence: instructionSequence ]
]
