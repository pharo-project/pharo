Class {
	#name : #HERingToHermesBuilder,
	#superclass : #Object,
	#category : 'Hermes-Ring'
}

{ #category : #compiling }
HERingToHermesBuilder >> compileMethod: aRFiMethod [
	^ OpalCompiler new
		class: (HERingBinder onMethod: aRFiMethod);
		source: aRFiMethod sourceCode;
		compile
]

{ #category : #visiting }
HERingToHermesBuilder >> visitClass: aClass [
	| aHEClass |
	
	aHEClass := HEClass new.
	self visitMethodContainer: aClass using: aHEClass.

	aHEClass className: aClass name.
	aHEClass superclass: (aClass superclass ifNil:'' ifNotNil:#name).
	aHEClass instanceVariables: (aClass instanceVariables joinUsing: ' ').
	aHEClass classInstancevariables: (aClass theMetaClass instanceVariables joinUsing: ' ').
	aHEClass classVariables: (aClass classPool keys joinUsing: ' ').
	aHEClass sharedPools: ((aClass sharedPools collect: #name) joinUsing: ' ').
	aHEClass layoutClass: aClass ficusClass layout realClassName.
	
	^ aHEClass.
]

{ #category : #visiting }
HERingToHermesBuilder >> visitMethod: aMethod [
	| aHEMethod compiledMethod |
	aHEMethod  := HEMethod new.
	aHEMethod name: aMethod selector.
	aHEMethod className: aMethod methodClass name.
	aHEMethod protocol: aMethod protocol.

	compiledMethod := self compileMethod: aMethod.

	aHEMethod fillLiteralsAndBytecode: compiledMethod.
	
	^ aHEMethod
]

{ #category : #visiting }
HERingToHermesBuilder >> visitMethodContainer: aMethodContainer using: aHEClass [
	aHEClass category: aMethodContainer category.
	aHEClass traitComposition: aMethodContainer traitComposition asExportedLiteral.
	aHEClass classTraitComposition: aMethodContainer theMetaClass traitComposition asExportedLiteral.

	aHEClass methods: (aMethodContainer localMethods collect: [ :e | e acceptVisitor: self ]) asArray.
	aHEClass classSideMethods: (aMethodContainer theMetaClass localMethods collect: [ :e | e acceptVisitor: self])
]

{ #category : #visiting }
HERingToHermesBuilder >> visitPackage: aPackage [
	| aHEPackage |
	aHEPackage := HEPackage new.

	aHEPackage packageName: aPackage name.
	aHEPackage traits:((aPackage definedClasses select: #isTrait) collect: [ :e | e acceptVisitor:self ] as: OrderedCollection).

	(aPackage definedClasses reject: #isTrait) do: [ :e | aHEPackage addClass: (e acceptVisitor:self) ].
	aPackage extensionMethods do: [ :e | aHEPackage extensionMethods add: (e acceptVisitor:self) ].
	
	^ aHEPackage.
]

{ #category : #visiting }
HERingToHermesBuilder >> visitTrait: aTrait [
	| aHETrait |
	
	aHETrait := HETrait new.
	self visitMethodContainer: aTrait using: aHETrait.

	aHETrait traitName: aTrait name.
	
	^ aHETrait.
]
