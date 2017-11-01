"
I am a slot in a class. 

Actual code generation is forwarded to the Slot class.

e.g. an InstanceVariableSlot will generate bytecode for reading and writing an ivar, while other Slots either just call the reflective operations of the Slot (#read: and #write:to) or do their own code generation.
"
Class {
	#name : #OCSlotVariable,
	#superclass : #OCAbstractVariable,
	#instVars : [
		'slot'
	],
	#category : 'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCSlotVariable class >> semanticNodeClass [

	^RBInstanceVariableNode
]

{ #category : #emitting }
OCSlotVariable >> emitStore: methodBuilder [

	slot emitStore: methodBuilder
]

{ #category : #emitting }
OCSlotVariable >> emitValue: methodBuilder [

	slot emitValue: methodBuilder
]

{ #category : #testing }
OCSlotVariable >> isInstance [
	^ true
]

{ #category : #accessing }
OCSlotVariable >> name [
	^ slot name
]

{ #category : #accessing }
OCSlotVariable >> name: aString [
	slot name: aString asSymbol
]

{ #category : #accessing }
OCSlotVariable >> slot [
	^ slot
]

{ #category : #accessing }
OCSlotVariable >> slot: aSlot [
	slot := aSlot
]

{ #category : #accessing }
OCSlotVariable >> variable [
	^slot
]
