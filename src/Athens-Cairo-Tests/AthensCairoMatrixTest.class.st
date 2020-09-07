"
A test for AthensCairoMatrix
"
Class {
	#name : #AthensCairoMatrixTest,
	#superclass : #AthensTransformTest,
	#category : #'Athens-Cairo-Tests-Transforms'
}

{ #category : #tests }
AthensCairoMatrixTest >> testCopy [

	| m |
	m := AthensCairoMatrix new.
	self assert: m getHandle equals: m copy getHandle.
	self assert: m getHandle ~~ m copy getHandle
]

{ #category : #tests }
AthensCairoMatrixTest >> testInverted [

	| m |
	m := AthensCairoMatrix new.
	self assert: m getHandle ~~ m inverted getHandle
]

{ #category : #tests }
AthensCairoMatrixTest >> transformClass [
	^ AthensCairoMatrix
]
