Class {
	#name : 'ShTraitInstallerTest',
	#superclass : 'ShClassInstallerTest',
	#category : 'Traits-Tests-ShiftClassInstaller',
	#package : 'Traits-Tests',
	#tag : 'ShiftClassInstaller'
}

{ #category : 'tests' }
ShTraitInstallerTest >> testAddingATraitToAClassHasRightProtocolsOnMetaclass [
	"Regression test. Some methods were copied on the method dic of a metaclass but the protocols were lost"

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #SHClass;
			            package: self generatedClassesPackageName ].

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #SHClass;
			            traits: { TViewModelMock };
			            package: self generatedClassesPackageName ].

	self assert: (newClass methods allSatisfy: [ :method | method protocol isNotNil ]).
	self assert: (newClass class methods allSatisfy: [ :method | method protocol isNotNil ]).
	self assert: (newClass class class methods allSatisfy: [ :method | method protocol isNotNil ])
]

{ #category : 'tests' }
ShTraitInstallerTest >> testCreatingFullTraitHasAllElements [

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            beTrait;
			            name: #TSHClass;
			            slots: { #a. #b };
			            traits: { TViewModelMock };
			            tag: 'boring';
			            package: self generatedClassesPackageName ].

	self assert: newClass name equals: #TSHClass.
	self assert: newClass slots size equals: 2.
	self assert: newClass slotNames equals: #( a b ).
	self assert: newClass traitComposition equals: { TViewModelMock } asTraitComposition.
	self assert: newClass class traitComposition equals: { TViewModelMock classSide } asTraitComposition.
	self assert: newClass package name equals: self generatedClassesPackageName.
	self assert: newClass packageTag name equals: 'boring'
]

{ #category : 'tests' }
ShTraitInstallerTest >> testRemovingTraitCompositionOfAClassShouldUpdateItsMetaclass [

	| t1 |
	t1 := ShiftClassInstaller make: [ :builder |
		      builder
			      name: #TShCITestClass;
			      beTrait;
			      package: self generatedClassesPackageName ].
	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #ShCITestClass;
			            traits: t1;
			            package: self generatedClassesPackageName ].

	self assert: newClass class class equals: TraitedMetaclass.

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #ShCITestClass;
			            package: self generatedClassesPackageName ].

	self assert: newClass class class equals: Metaclass
]

{ #category : 'tests' }
ShTraitInstallerTest >> testRemovingTraitCompositionOfAClassThroughAnUpdateShouldUpdateItsMetaclass [

	| t1 |
	t1 := ShiftClassInstaller make: [ :builder |
		      builder
			      name: #TShCITestClass;
			      beTrait;
			      package: self generatedClassesPackageName ].

	newClass := ShiftClassInstaller make: [ :builder |
		            builder
			            name: #ShCITestClass;
			            traits: t1;
			            package: self generatedClassesPackageName ].

	self assert: newClass class class equals: TraitedMetaclass.

	newClass := ShiftClassInstaller update: newClass to: [ :builder | builder traits: {  } ].

	self assert: newClass class class equals: Metaclass
]
