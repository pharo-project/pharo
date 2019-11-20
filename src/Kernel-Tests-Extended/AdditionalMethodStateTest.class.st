"
SUnit tests for AdditionalMethodState
"
Class {
	#name : #AdditionalMethodStateTest,
	#superclass : #TestCase,
	#instVars : [
		'amState'
	],
	#category : #'Kernel-Tests-Extended-Methods'
}

{ #category : #running }
AdditionalMethodStateTest >> setUp [

	| pragma |
	super setUp.
	pragma := (Object compiledMethodAt: #at:) penultimateLiteral at: #primitive:.
	
	amState := AdditionalMethodState selector: #at: with: pragma copy.

]

{ #category : #tests }
AdditionalMethodStateTest >> testAnalogousCodeTo [
	"create a fake traitSource association property"
	| state |
	state := AdditionalMethodState new: 1.
	state 
		basicAt: 1
		put: #traitSource -> TSortable.
		
	self 
		shouldnt: [ state analogousCodeTo: state ]
		raise: MessageNotUnderstood
]

{ #category : #tests }
AdditionalMethodStateTest >> testCopy [
	| copy |
	copy := amState copy.

	self deny: amState identicalTo: copy.
	self assert: amState method identicalTo: copy method.
	self assert: amState selector identicalTo: copy selector.

	self assert: amState pragmas equals: copy pragmas.
	self assert: amState properties equals: copy properties.

	amState pragmas withIndexDo: [ :el :index | self deny: el identicalTo: (copy pragmas at: index) ]
]
