Class {
	#name : #HEInstallerTest,
	#superclass : #HEAbstractBinaryClassBuilderTest,
	#instVars : [
		'builder',
		'aClass',
		'aTrait'
	],
	#category : 'Hermes-Tests'
}

{ #category : #utils }
HEInstallerTest >> changeNameOf: exportedClass to: aNewName [
	(exportedClass isKindOf: HETrait)
		ifTrue: [ 
				exportedClass methods do:[:e | self renameExportedClass: exportedClass traitName to: aNewName on:e ].
				exportedClass instVarNamed: #traitName put: aNewName. 
			]
		ifFalse: [ 
			exportedClass methods do:[:e | self renameExportedClass: exportedClass className to: aNewName on:e ].			
			exportedClass instVarNamed: #className put: aNewName ].
		

]

{ #category : #utils }
HEInstallerTest >> renameExportedClass: aName to: aNewName on:aMethod [
	aMethod literals do:[ :e |
		(e isKindOf: HEAdditionalMethodState) ifTrue:[
			e value value do:[:x | 
				(x method className = aName) ifTrue: [ x method className: aNewName ]					
			].
		]
	].
]

{ #category : #initialization }
HEInstallerTest >> setUp [
	super setUp.
	builder := HEInstaller new.
]

{ #category : #initialization }
HEInstallerTest >> tearDown [
	aClass ifNotNil: #removeFromSystem.
	aTrait ifNotNil: #removeFromSystem.

	UndefinedObject removeSelector: #xxx.

	super tearDown.

]

{ #category : #tests }
HEInstallerTest >> testCreatingAClassUsingComplexTraits [
	| exportedClass obj|
	
	exportedClass := HEClass for: HETeSimpleClassWithComplexTraits. 
	self changeNameOf: exportedClass to: #HETeSimpleClassWithComplexTraitsNew.
	
	aClass := builder build: exportedClass.
	
	self assert: aClass name equals: #HETeSimpleClassWithComplexTraitsNew.
	self assert: aClass class name equals: #'HETeSimpleClassWithComplexTraitsNew class'.

	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self deny: aClass traitComposition isEmpty.
	self deny: aClass class traitComposition isEmpty.
		
	obj := aClass new.
	
	self assert: obj oneMessage equals: 1234.
	self assert: obj otherMessage equals: 4321.	
]

{ #category : #tests }
HEInstallerTest >> testCreatingAClassUsingTraits [
	| exportedClass obj|
	
	exportedClass := HEClass for: HETeSimpleClassWithTraits. 
	self changeNameOf: exportedClass to: #HETeSimpleClassWithTraitsNew.
	
	aClass := builder build: exportedClass.
	
	self assert: aClass name equals: #HETeSimpleClassWithTraitsNew.
	self assert: aClass class name equals: #'HETeSimpleClassWithTraitsNew class'.

	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self deny: aClass traitComposition isEmpty.
	self deny: aClass class traitComposition isEmpty.
		
	obj := aClass someClassSideMessage.

	self assert: aClass slots isEmpty.	
	self assert: aClass class slots isEmpty.	

	self assert: obj oneMessage equals: 1234.
	self assert: obj otherMessage equals: 4321.	
]

{ #category : #tests }
HEInstallerTest >> testCreatingASimpleClass [
	| exportedClass slots classSlots obj |
	
	exportedClass := HEClass for: HETeSimpleClass. 
	self changeNameOf: exportedClass to: #HETeSimpleClassNew.
	
	aClass := builder build: exportedClass.
	
	self assert: aClass name equals: #HETeSimpleClassNew.
	self assert: aClass class name equals: #'HETeSimpleClassNew class'.
	
	slots :=  {#anInstanceVariable => InstanceVariableSlot}.
	(slots at:1) index: 1.	
	self assert: aClass slots equals: slots.
	
	classSlots :=  {#aClassInstanceVariable => InstanceVariableSlot}.
	(classSlots at:1) index: (Object class allSlots size + 1).		
	self assert: aClass class slots equals: classSlots.
	
	self assert: aClass traitComposition isEmpty.
	self assert: aClass classVariables equals: { #AClassVariable => ClassVariable }.
	
	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self assert: aClass traitComposition isEmpty.
	self assert: aClass class traitComposition isEmpty.

	obj := aClass newInstance.
	
	obj anInstanceVariable: 25.
	self assert: obj anInstanceVariable equals: 25.
	self assert: obj otherMethod equals: 25.
	self assert: obj usingAConstant equals: 42.
]

{ #category : #tests }
HEInstallerTest >> testCreatingASimpleClassExportingToFile [
	| exportedClass obj readedClass |
	
	exportedClass := HEClass for: HETeSimpleClass. 
	self changeNameOf: exportedClass to: #HETeSimpleClassNew.

	exportedClass writeInto: writer.
	writer flush.
	
	readedClass := HEClass readFrom: reader.

	self assert: readedClass equals: exportedClass.
	
	aClass := builder build: exportedClass.	
	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self assert: aClass traitComposition isEmpty.
	self assert: aClass class traitComposition isEmpty.

	obj := aClass newInstance.
	
	obj anInstanceVariable: 25.
	self assert: obj anInstanceVariable equals: 25.
	self assert: obj otherMethod equals: 25.
	self assert: obj usingAConstant equals: 42.
]

{ #category : #tests }
HEInstallerTest >> testCreatingASimpleTrait [
	| exportedTrait |
	
	exportedTrait := HETrait for: THEOneTestTrait. 
	self changeNameOf: exportedTrait to: #THEOneTestTraitNew.
	
	aTrait := builder buildTrait: exportedTrait.	
	builder installMethods: exportedTrait into: aTrait.

	self assert: aTrait traitComposition isEmpty.
	self assert: aTrait theMetaClass traitComposition isEmpty.
	
	self assert: aTrait methods size equals: THEOneTestTrait methods size.
	self assert: aTrait classTrait methods size equals: THEOneTestTrait classTrait methods size.	
	
	aTrait >> #oneMessage.
	aTrait >> #oneMessageRequired
]

{ #category : #tests }
HEInstallerTest >> testCreatingATraitUsingOthers [
	| exportedTrait |
	
	exportedTrait := HETrait for: THEATraitUsingOthers. 
	self changeNameOf: exportedTrait to: #THEATraitUsingOthersNew. 
	
	aTrait := builder buildTrait: exportedTrait.	
	builder installMethods: exportedTrait into: aTrait.

	self deny: aTrait traitComposition isEmpty.
	self deny: aTrait theMetaClass traitComposition isEmpty.
	
	self assert: aTrait methods size equals: THEATraitUsingOthers methods size.
	self assert: aTrait classTrait methods size equals: THEATraitUsingOthers classTrait methods size.	
		
	self assert: aTrait traitComposition printString equals: THEATraitUsingOthers traitComposition printString.

	self assert: aTrait classTrait traitComposition printString equals: THEATraitUsingOthers classTrait traitComposition printString.
	
	aTrait >> #oneMessage.
	aTrait >> #oneMessageRequired
]

{ #category : #tests }
HEInstallerTest >> testPragmaMethod [
	| exportedClass aMethod |
	
	exportedClass := HEClass for: HETeSimpleClass. 
	self changeNameOf: exportedClass to: #HETeSimpleClass2.
	
	aClass := builder build: exportedClass.
	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.
	
	aMethod := aClass >> #pragmaMethod.
	self assert: aMethod pragmas first method == aMethod. 
]

{ #category : #tests }
HEInstallerTest >> testUndeclared [
	| exportedClass |
	
	UndefinedObject compile:'xxx
		^ AnUndeclaredClass'.
		
	self assert: nil xxx equals: nil.
	
	exportedClass := HEClass for: HETeSimpleClass. 
	self changeNameOf: exportedClass to: #AnUndeclaredClass.

	aClass := builder build: exportedClass.
	builder installClass: aClass in: exportedClass category.
	builder installMethods: exportedClass into: aClass.

	self should: [ Undeclared at: #AnUndeclaredClass ] raise: KeyNotFound.
	Smalltalk at:#AnUndeclaredClass.
]
