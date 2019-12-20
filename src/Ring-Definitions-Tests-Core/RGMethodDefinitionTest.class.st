"
SUnit tests for method definitions
"
Class {
	#name : #RGMethodDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Core-Base'
}

{ #category : #running }
RGMethodDefinitionTest >> runCase [

	SystemAnnouncer uniqueInstance suspendAllWhile: [ 
		Author useAuthor: self class name asString during: [ super runCase ]
	]

]

{ #category : #running }
RGMethodDefinitionTest >> tearDown [

	self class removeSelectorSilently: #foo.
	self class removeProtocol: 'test'.
	super tearDown 
]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testASTExistingMethod [
	"accessing ring methods ast from active method"

	| method |
	method := (RGMethodDefinition class >> #class:selector:) asRingDefinition.
	self assert: method isActive.
	self assert: method ast equals: method compiledMethod ast.
]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testASTNonExistingMethodWithNoSource [
	"accessing ring methods ast for passive ring definition with no source"

	| newMethod newClass |
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #foo) parent: newClass.
	self assert: newMethod isPassive.
	self assert: newMethod ast isNil.
]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testASTNonExistingMethodWithSource [
	"accessing ring methods ast for passive ring definition created with source"

	| newMethod newClass source |
	source := 'foo
   ^ self'.
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #foo) parent: newClass.
	newMethod sourceCode: source.
	self assert: newMethod isPassive.
	self assert: newMethod ast equals: (RBParser parseMethod: source)

]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testArgumentNamesExistingMethod [
	"accessing ring methods arguments from active method"

	| method |
	method := (RGMethodDefinition class >> #class:selector:) asRingDefinition.
	self assert: method isActive.
	self assertCollection: method argumentNames  hasSameElements: #(#aRGBehaviorDefinition #aString) 
]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testArgumentNamesNonExistingMethodWithNoSource [
	"accessing ring methods arguments for passive ring definition created with source"

	| newMethod newClass |
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #foo:) parent: newClass.
	self assert: newMethod isPassive.
	self assert: newMethod argumentNames isNil.

]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testArgumentNamesNonExistingMethodWithSource [
	"accessing ring methods arguments for passive ring definition created with source"

	| newMethod newClass source |
	source := 'class: aClass selector: aSelector
              "new passive method"'.
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #class:selector:) parent: newClass.
	newMethod sourceCode: source.
	self assert: newMethod isPassive.
	self assertCollection: newMethod argumentNames hasSameElements: #( #aClass #aSelector).

]

{ #category : #testing }
RGMethodDefinitionTest >> testAsActive [
	| newMethod |
	RGMethodDefinitionTest compileSilently: 'foo	^ ''first version''' classified: 'test'.
	newMethod := (RGMethodDefinitionTest >> #foo) asActiveRingDefinition.
	self assert: newMethod isActive.
	self assert: newMethod sourcePointer notNil.
	self assert: newMethod sourceCode equals: newMethod compiledMethod sourceCode.

	RGMethodDefinitionTest compileSilently: 'foo	^ ''second version''' classified: 'test'.
	self assert: newMethod sourceCode equals: newMethod compiledMethod sourceCode.

	newMethod := (Class >> #asRingDefinition) asActiveRingDefinition.
	newMethod fromActiveToPassive.
	self assert: newMethod isPassive.
	self assert: newMethod sourceCode equals: (Class >> #asRingDefinition) sourceCode.
	newMethod sourceCode: 'asRingDefinition   ^true'.
	self assert: newMethod sourceCode ~= (Class >> #asRingDefinition) sourceCode.

	newMethod := (Class >> #asRingDefinition) asActiveRingDefinition.
	newMethod fromActiveToHistorical.
	self assert: newMethod isHistorical.
	self assert: newMethod sourcePointer notNil
]

{ #category : #testing }
RGMethodDefinitionTest >> testAsHistorical [
	| firstVersion secondVersion |
	RGMethodDefinitionTest compileSilently: 'foo	^ ''first version''' classified: 'test'.
	firstVersion := (RGMethodDefinitionTest >> #foo) asHistoricalRingDefinition.
	self assert: firstVersion isHistorical.
	self assert: firstVersion sourceCode equals: firstVersion compiledMethod sourceCode.
	self assert: firstVersion stamp equals: firstVersion compiledMethod timeStamp.

	RGMethodDefinitionTest compileSilently: 'foo	^ ''second version''' classified: 'test'.
	secondVersion := (RGMethodDefinitionTest >> #foo) asHistoricalRingDefinition.
	self assert: secondVersion isHistorical.
	self assert: secondVersion sourceCode equals: secondVersion compiledMethod sourceCode.
	self assert: secondVersion stamp equals: secondVersion compiledMethod timeStamp.

	self deny: firstVersion sourcePointer equals: secondVersion sourcePointer.
	self deny: firstVersion sourceCode equals: secondVersion sourceCode
]

{ #category : #testing }
RGMethodDefinitionTest >> testAsMethodDefinition [
	| newMethod |
	newMethod := (OrderedCollection >> #size) asRingDefinition.

	self assert: newMethod isRingObject.
	self assert: newMethod parent isNil.
	self assert: newMethod isMethod.
	self assert: newMethod selector identicalTo: #size.
	self assert: newMethod protocol notNil.
	self assert: newMethod stamp notNil
]

{ #category : #testing }
RGMethodDefinitionTest >> testAsMethodDefinition2 [
	| newMethod |
	newMethod := (Object >> #printOn:) asRingDefinition.
	self assert: newMethod protocol equals: 'printing'.

	newMethod := (TCloneTest >> #testCopyEmpty) asRingDefinition.
	self assert: newMethod protocol equals: 'tests - copy - clone'.

	newMethod := (ArrayTest >> #testCopyEmpty) asRingDefinition.
	self assert: newMethod protocol equals: 'tests - copy - clone'
]

{ #category : #testing }
RGMethodDefinitionTest >> testAsPassive [
	| newMethod |
	RGMethodDefinitionTest compileSilently: 'foo	^ ''first version''' classified: 'test'.
	newMethod := (RGMethodDefinitionTest >> #foo) asPassiveRingDefinition.
	self assert: newMethod isPassive.
	self assert: newMethod sourceCode equals: newMethod compiledMethod sourceCode.

	RGMethodDefinitionTest compileSilently: 'foo	^ ''second version''' classified: 'test'.
	self assert: newMethod sourceCode equals: 'foo	^ ''first version'''.
	self assert: newMethod compiledMethod sourceCode equals: 'foo	^ ''second version'''
]

{ #category : #testing }
RGMethodDefinitionTest >> testClassNameSelectorIsMetaSide [
	| rg |
	rg := RGMethodDefinition className: 'Point' selector: #x isMetaSide: false.
	self assert: rg parentName equals: 'Point'.
	self assert: (rg parent isKindOf: RGClassDefinition).
	self assert: rg selector equals: #x.
	self assert: rg isMetaSide not
]

{ #category : #testing }
RGMethodDefinitionTest >> testCreatingMethodsWithoutFactory [
	| newMethod newClass |
	newMethod := RGMethodDefinition realClass: OrderedCollection selector: 'size'.
	self assert: newMethod isMethod.
	self assert: newMethod selector identicalTo: #size.
	self assert: newMethod protocol notNil.
	self assert: newMethod sourceCode notEmpty.
	self assert: newMethod stamp notNil.

	newClass := RGClassDefinition named: #OrderedCollection.
	newMethod := RGMethodDefinition class: newClass selector: 'size'.
	self assert: newMethod isMethod.
	self assert: newMethod selector identicalTo: #size.
	self assert: newMethod protocol isNil.
	self assert: newMethod sourceCode isNil.
	self assert: newMethod stamp isNil.

	newMethod := RGMethodDefinition realClass: OrderedCollection classSide selector: #arrayType.
	self assert: newMethod isMethod.
	self assert: newMethod selector identicalTo: #arrayType.
	self assert: newMethod protocol notNil.
	self assert: newMethod sourceCode notEmpty.
	self assert: newMethod stamp notNil
]

{ #category : #testing }
RGMethodDefinitionTest >> testDifferentObjectButSameMethodInSet [
	| s rgmethod rgmethod2 |
	s := Set new.
	rgmethod := (OrderedCollection >> #size) asRingDefinition.
	s add: rgmethod.
	rgmethod2 := (OrderedCollection >> #size) asRingDefinition.
	s add: rgmethod2.

	self assert: s size equals: 1.
	self assert: (s includes: rgmethod).
	self assert: (s includes: rgmethod2)
]

{ #category : #testing }
RGMethodDefinitionTest >> testExistingMethodWithClass [
	| newMethod newClass |
	newClass := RGClassDefinition named: #OrderedCollection.
	newMethod := (RGMethodDefinition named: #add:)
		parent: newClass;
		protocol: 'adding';
		sourceCode:
			'add: newObject
									^self addLast: newObject'.

	self assert: newMethod isMethod.
	self assert: newMethod selector identicalTo: #add:.
	self assert: newMethod isMetaSide not.

	self assert: newMethod parent equals: newClass.
	self assert: newMethod parentName identicalTo: newClass name.

	self assert: newMethod parent parent identicalTo: Smalltalk globals.
	self assert: newMethod realClass equals: OrderedCollection.
	self assert: newMethod compiledMethod equals: (OrderedCollection compiledMethodAt: #add:)
]

{ #category : #testing }
RGMethodDefinitionTest >> testExistingMethodWithPointer [
	| newMethod |
	newMethod := (OrderedCollection >> #size) asActiveRingDefinition.

	self assert: newMethod parent isNil.
	self assert: newMethod parentName identicalTo: #OrderedCollection.
	self assert: newMethod sourcePointer notNil.
	self assert: newMethod sourceCode notNil.
	self assert: newMethod protocol notNil.
	self assert: newMethod stamp notNil.
	self assert: newMethod isExtension not
]

{ #category : #testing }
RGMethodDefinitionTest >> testExistingMethodWithoutClass [
	"its parent class is not assigned only its name"

	| newMethod |
	newMethod := (RGMethodDefinition named: #add:)
		parentName: #OrderedCollection;
		selector: #add:;
		isMetaSide: false;
		protocol: 'adding';
		sourceCode:
			'add: newObject
									^self addLast: newObject'.

	self assert: newMethod isMethod.
	self assert: newMethod selector identicalTo: #add:.
	self assert: newMethod isMetaSide not.
	self assert: newMethod protocol equals: #adding.
	self assert: newMethod fullName equals: 'OrderedCollection>>add:'.
	self
		assert: newMethod sourceCode
		equals:
			'add: newObject
									^self addLast: newObject'.
	self assert: newMethod hasStamp not.
	self assert: newMethod parent isNil.
	self assert: newMethod parentName identicalTo: #OrderedCollection.

	self assert: newMethod environment identicalTo: Smalltalk globals.
	self assert: newMethod realClass equals: OrderedCollection.
	self assert: newMethod compiledMethod equals: (OrderedCollection compiledMethodAt: #add:)
]

{ #category : #testing }
RGMethodDefinitionTest >> testExtensionMethod [
	| newMethod newClass newPackage newPackageExt |

	newMethod:= (RGMethodDefinition named: #+)
					isExtension: true.
	self assert: (newMethod protocol isNil).
	self assert: (newMethod isExtension).

	newMethod:= (Collection >> #+) asRingDefinition.	
	self assert: (newMethod isMethod).  
	self assert: newMethod protocol equals: '*Collections-arithmetic'.
	self assert: (newMethod isExtension).
	
	newPackage := RGPackageDefinition named: #Package.
	newPackageExt := RGPackageDefinition named:  #PackageExtensions.
	newClass := RGClassDefinition named: #Class.
	newMethod := (RGMethodDefinition named: #foo) parent: newClass.
	newPackage addClass: newClass.
	newMethod package: newPackageExt.
	self assert: (newMethod isExtension).
]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testIsAbstractExistingMethod [
	"testing existing ring method isAbstract"

	| method |
	method := (RGMethodDefinition class >> #class:selector:) asRingDefinition.
	self assert: method isActive.
	self deny: method isAbstract.
]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testIsAbstractNonExistingMethod [
	"testing non existing ring method isAbstract"

	| newMethod newClass |
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #foo:) parent: newClass.
	self assert: newMethod isPassive.
	self deny: newMethod isAbstract.

]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testMessagesExistingMethod [
	"accessing ring sent messages for active method"

	| method |
	method := (RGMethodDefinition class >> #class:selector:) asRingDefinition.
	self assert: method isActive.
	self assert: method messages equals: method compiledMethod messages.
]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testMessagesNonExistingMethodWithNoSource [
	"accessing ring sent messages for passive method with no source"

	| newMethod newClass |
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #class:selector:) parent: newClass.
	self assert: newMethod isPassive.
	self assert: newMethod messages equals: #()

]

{ #category : #'test - accessing source' }
RGMethodDefinitionTest >> testMessagesNonExistingMethodWithSource [
	"accessing ring sent messages for passive method created with source"

	| newMethod newClass source |
	source := 'class: aClass selector: aSelector
              ^ self new; yourself'.
	newClass := RGClassDefinition named: #RGMethodDefinition.
	newMethod := (RGMethodDefinition named: #class:selector:) parent: newClass.
	newMethod sourceCode: source.
	self assert: newMethod isPassive.
	self assertCollection: newMethod messages hasSameElements: #( #new #yourself).

]

{ #category : #testing }
RGMethodDefinitionTest >> testMethodEquality [
	| newMethod newClass |
	self assert: (OrderedCollection >> #size) asRingDefinition equals: (OrderedCollection >> #size) asRingDefinition.

	newMethod := (OrderedCollection >> #size) asRingDefinition
		sourceCode:
			'size
						^0'.
	self assert: (OrderedCollection >> #size) asRingDefinition equals: newMethod.

	newClass := RGClassDefinition named: #OrderedCollection.
	newMethod := (RGMethodDefinition named: #size) parent: newClass.
	self assert: (OrderedCollection >> #size) asRingDefinition equals: newMethod.

	newMethod := (RGMethodDefinition named: #size) parent: SortedCollection asRingDefinition.
	self deny: (OrderedCollection >> #size) asRingDefinition equals: newMethod
]

{ #category : #testing }
RGMethodDefinitionTest >> testNonExistingMethodWithClass [
	"method does not exist in OrderedCollection"

	| newMethod newClass |
	newClass := RGClassDefinition named: #OrderedCollection.
	newClass withMetaclass.

	newMethod := (RGMethodDefinition named: #foo) parent: newClass classSide.
	newMethod
		sourceCode:
			'foo
									^true'.

	self assert: newMethod isMethod.
	self assert: newMethod selector identicalTo: #foo.
	self assert: newMethod isMetaSide.
	self assert: newMethod protocol equals: nil.

	self assert: newMethod parent equals: newClass classSide.
	self assert: newMethod parentName equals: newClass classSide name.

	self assert: newMethod environment identicalTo: Smalltalk globals.
	self assert: newMethod realClass equals: OrderedCollection class.
	self assert: newMethod compiledMethod isNil
]

{ #category : #testing }
RGMethodDefinitionTest >> testSameMethodInSet [
	| s rgmethod |
	s := Set new.
	rgmethod := (OrderedCollection >> #size) asRingDefinition.
	s add: rgmethod.
	self assert: (s includes: rgmethod).

	s add: rgmethod.
	self assert: s size equals: 1.
	self assert: (s includes: rgmethod)
]

{ #category : #testing }
RGMethodDefinitionTest >> testSorting [

	| rgMethod1 rgMethod2 |
	rgMethod1 := RGMethodDefinition realClass: RGInstanceVariableDefinition selector: #isInstanceVariable.
	rgMethod2 := RGMethodDefinition realClass: RGElementDefinition selector: #isMetaSide.
	self assert: rgMethod2 <= rgMethod1.
	
	rgMethod1 := RGMethodDefinition realClass: RGElementDefinition selector: #parentName.
	self assert: rgMethod2 <= rgMethod1. 
	
]
