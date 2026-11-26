"
I check how a `TransformMorph` draws its submorphs through its transform.
"
Class {
	#name : 'TransformMorphTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Tests-Kernel',
	#package : 'Morphic-Tests',
	#tag : 'Kernel'
}

{ #category : 'tests' }
TransformMorphTest >> testScaledDrawsItsSubmorphs [

	| transformMorph form |
	transformMorph := TransformMorph new.
	transformMorph
		color: Color transparent;
		bounds: (0 @ 0 extent: 20 @ 20).
	transformMorph addMorph: (Morph new
			 color: Color red;
			 bounds: (0 @ 0 extent: 5 @ 5);
			 yourself).
	transformMorph scale: 2.
	form := Form extent: 20 @ 20 depth: 32.

	transformMorph fullDrawOn: form getCanvas.

	self assert: (form colorAt: 8 @ 8) equals: Color red
]
