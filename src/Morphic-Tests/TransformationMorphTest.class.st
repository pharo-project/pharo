"
I check TransformationMorph. Each test builds its own root morph, not the world.
"
Class {
	#name : 'TransformationMorphTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Tests-Basic',
	#package : 'Morphic-Tests',
	#tag : 'Basic'
}

{ #category : 'tests' }
TransformationMorphTest >> testFlexingWrapsTheMorphInItsPlace [

	| root before morph after shell |
	root := Morph new.
	before := Morph new.
	morph := Morph new.
	after := Morph new.
	root
		addMorphBack: before;
		addMorphBack: morph;
		addMorphBack: after.

	shell := TransformationMorph flexing: morph.

	self assert: root submorphs asArray equals: {
		before.
		shell.
		after }.
	self assert: morph owner identicalTo: shell.
	self assert: morph topRendererOrSelf identicalTo: shell
]
