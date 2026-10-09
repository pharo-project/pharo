Class {
	#name : 'RBCopyPackageParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'building suites' }
RBCopyPackageParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBCopyPackageRefactoring };
		yourself
]

{ #category : 'accessing' }
RBCopyPackageParametrizedTest >> constructor [
	^ #copyPackage:in:
]

{ #category : 'running' }
RBCopyPackageParametrizedTest >> setUp [

	super setUp.
	model := self defaultNamespaceClass new.

	#( #RBFTTest1 #RBFTTest2 ) do: [ :className |
		(RBInsertNewClassTransformation className: className)
			superclass: #Object;
			packageName: #RefactoringSmallPackageForTest;
			execute ].

	(RBAddMethodTransformation
		 sourceCode: 'foo
^ RBFTTest2'
		 in: #RBFTTest1
		 withProtocol: #accessing) execute
]

{ #category : 'running' }
RBCopyPackageParametrizedTest >> tearDown [

	(ReChangeFactory instance removePackageNamed: #RefactoringSmallPackageForTest) execute.
	(ReChangeFactory instance removePackageNamed: #RefactoringCOPYSmallPackageForTest) execute.
	super tearDown
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageAndChangesCopyReferences [

	| refactoring aModel |
	refactoring := self createRefactoringWithArguments: { #RefactoringSmallPackageForTest. #RefactoringCOPYSmallPackageForTest }.
	self executeRefactoring: (refactoring classMappings: {
				 (#RBFTTest1 -> #BarFTTest1).
				 (#RBFTTest2 -> #BarFTTest2) } asDictionary).
	aModel := refactoring model.

	self assert: (aModel classNamed: #BarFTTest1) packageName equals: #RefactoringCOPYSmallPackageForTest.
	self assert: (aModel classNamed: #BarFTTest2) superclass name equals: #Object.

	self assert: ((aModel classNamed: #BarFTTest1) parseTreeForSelector: #foo) equals: (self parseMethod: 'foo
			^ BarFTTest2')
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageWithParameters [

	| refactoring aModel |
	refactoring := self createRefactoringWithArguments: { #RefactoringSmallPackageForTest. #RefactoringCOPYSmallPackageForTest }.
	self executeRefactoring: (refactoring classMappings: {
				 (#RBFTTest1 -> #BarFTTest1).
				 (#RBFTTest2 -> #BarFTTest2) } asDictionary).
	aModel := refactoring model.
	self assert: (aModel includesClassNamed: #BarFTTest1) description: 'It test that class was copied'.
	self assert: (aModel includesClassNamed: #BarFTTest2) description: 'It test that class was copied'.
	self assert: (aModel includesClassNamed: #RBFTTest1) description: 'It test that original class where we copied from is still there'.
	self assert: (aModel includesClassNamed: #RBFTTest2) description: 'It test that original class where we copied from is still there'
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageWithTrait [

	| duplicate refactoring |
	refactoring := self createRefactoringWithArguments: {
		               TReTraitToBeUsedInDuplication packageName.
		               #RefactoringCOPYSmallPackageForTest }.
	self executeRefactoring: (refactoring classMappings: {
			 #TReTraitToBeUsedInDuplication -> #TReDuplicatedTrait } asDictionary).

	duplicate := refactoring model classNamed: #TReDuplicatedTrait.
	self assert: duplicate class equals: RBTrait.
	self assert: duplicate superclass isNil.
	self assert: (duplicate directlyDefinesMethod: #methodInTrait).
	self
		assert: duplicate packageName
		equals: #RefactoringCOPYSmallPackageForTest
]

{ #category : 'failure tests' }
RBCopyPackageParametrizedTest >> testFailureBadName [

	self shouldFail: (self createRefactoringWithArguments: { #'Refactoring-Core-Tests'. #'Refactoring-Core-Tests' })
]

{ #category : 'failure tests' }
RBCopyPackageParametrizedTest >> testFailureExistingPackage [

	self shouldFail: (self createRefactoringWithArguments: { #'Refactoring-Core-Tests'. #'Refactoring-Changes-Tests' })
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageWithComposedTraitPreservesBothMethodSides [

	| duplicate refactoring undo |
	refactoring := self createRefactoringWithArguments: {
		TReComposedTraitForDuplication packageName.
		#RefactoringCOPYSmallPackageForTest }.
	self executeRefactoring: (refactoring classMappings: {
		#TReComposedTraitForDuplication -> #TReDuplicatedComposedTrait } asDictionary).
	duplicate := refactoring model classNamed: #TReDuplicatedComposedTrait.
	self assert: duplicate isTrait.
	self assertCollection: duplicate traitComposition traits
		hasSameElements: { TReTraitToBeUsedInDuplication }.
	self assert: (duplicate directlyDefinesMethod: #instanceSideValue).
	self assert: (duplicate classSide directlyDefinesMethod: #classSideValue).
	undo := refactoring changes execute.
	[
		| installed |
		installed := Smalltalk globals at: #TReDuplicatedComposedTrait.
		self assert: installed isTrait.
		self assert: installed packageName equals: #RefactoringCOPYSmallPackageForTest.
		self assertCollection: installed traits
			hasSameElements: { TReTraitToBeUsedInDuplication }.
		self assert: (installed includesSelector: #methodInTrait).
		self assert: installed classSideValue equals: 42
	] ensure: [ undo execute ].
	self deny: (Smalltalk globals includesKey: #TReDuplicatedComposedTrait)
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageRemapsCopiedTraitDependencies [

	| refactoring undo |
	refactoring := self createRefactoringWithArguments: {
		TReComposedTraitForDuplication packageName.
		#RefactoringCOPYSmallPackageForTest }.
	self executeRefactoring: (refactoring classMappings: {
		#TReComposedTraitForDuplication -> #TReDuplicatedComposedTrait.
		#TReTraitToBeUsedInDuplication -> #TReDuplicatedTrait } asDictionary).
	self assertCollection: ((refactoring model classNamed: #TReDuplicatedComposedTrait)
		traitComposition traits collect: #name)
		hasSameElements: #( TReDuplicatedTrait ).
	self assert: (refactoring model classNamed: #TReDuplicatedComposedTrait)
		traitComposition asString equals: 'TReDuplicatedTrait'.
	undo := refactoring changes execute.
	[
		| copiedTrait copiedDependency |
		copiedTrait := Smalltalk globals at: #TReDuplicatedComposedTrait.
		copiedDependency := Smalltalk globals at: #TReDuplicatedTrait.
		self assertCollection: copiedTrait traits hasSameElements: { copiedDependency }.
		self assertCollection: TReComposedTraitForDuplication traits
			hasSameElements: { TReTraitToBeUsedInDuplication }.
		self assert: copiedTrait classSideValue equals: 42.
		self assert: (copiedTrait includesSelector: #methodInTrait)
	] ensure: [ undo execute ].
	self deny: (Smalltalk globals includesKey: #TReDuplicatedComposedTrait).
	self deny: (Smalltalk globals includesKey: #TReDuplicatedTrait)
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackagePreservesAliasesAndExclusionsWhenRemappingTraits [

	| refactoring sourceComposition undo copiedComposition |
	refactoring := self createRefactoringWithArguments: {
		TReComposedTraitForDuplication packageName.
		#RefactoringCOPYSmallPackageForTest }.
	sourceComposition := TReTraitToBeUsedInDuplication
		@ { #aliasedMethod -> #methodInTrait } - { #methodInTrait }.
	(refactoring model classFor: TReComposedTraitForDuplication)
		traitComposition: sourceComposition.
	self executeRefactoring: (refactoring classMappings: {
		#TReComposedTraitForDuplication -> #TReDuplicatedComposedTrait.
		#TReTraitToBeUsedInDuplication -> #TReDuplicatedTrait } asDictionary).
	copiedComposition := (refactoring model classNamed: #TReDuplicatedComposedTrait)
		traitComposition.
	self assertCollection: (copiedComposition traits collect: #name)
		hasSameElements: #( TReDuplicatedTrait ).
	self assertCollection: sourceComposition traits
		hasSameElements: { TReTraitToBeUsedInDuplication }.
	undo := refactoring changes execute.
	[
		| copiedTrait copiedDependency |
		copiedTrait := Smalltalk globals at: #TReDuplicatedComposedTrait.
		copiedDependency := Smalltalk globals at: #TReDuplicatedTrait.
		self assertCollection: copiedTrait traits hasSameElements: { copiedDependency }.
		self assert: (copiedTrait includesSelector: #aliasedMethod).
		self deny: (copiedTrait includesSelector: #methodInTrait)
	] ensure: [ undo execute ]
]
