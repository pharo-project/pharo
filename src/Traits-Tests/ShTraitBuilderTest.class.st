Class {
	#name : 'ShTraitBuilderTest',
	#superclass : 'ShiftClassBuilderTest',
	#category : 'Traits-Tests-ShiftClassInstaller',
	#package : 'Traits-Tests',
	#tag : 'ShiftClassInstaller'
}

{ #category : 'tests' }
ShTraitBuilderTest >> testRemovingTraitCompositionOfAClassShouldUpdateItsMetaclass [

	| t1 newClass |
	t1 := (Trait << #TShCITestClass package: self packageNameForTest) install.

	newClass := ((Object << #ShCITestClass)
		             traits: t1;
		             package: self packageNameForTest) install.

	self assert: newClass class class equals: TraitedMetaclass.

	newClass := (Object << #ShCITestClass package: self packageNameForTest) install.

	self assert: newClass class class equals: Metaclass
]
