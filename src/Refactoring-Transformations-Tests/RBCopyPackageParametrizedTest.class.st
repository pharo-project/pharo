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
RBCopyPackageParametrizedTest >> testCopyPackagePreservesAliasesAndExclusionsWhenRemappingTraits [

	| refactoring sourceComposition copiedComposition |
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
	self
		assert: (OCParser parseExpression: copiedComposition traitCompositionExpression)
		equals: (OCParser parseExpression: 'TReDuplicatedTrait @ { (#aliasedMethod -> #methodInTrait) } - { #methodInTrait }')
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageInstallsCopiedTraitsUsingCopiedTraits [
	"Runs the changes in the image: the copied composed trait is created while the copied base trait may not exist yet."

	| base composed refactoring environment copiedBase copiedComposed |
	environment := self class environment.
	base := self class classInstaller make: [ :builder |
		builder
			beTrait;
			name: #TRBFTBase;
			package: #RefactoringSmallPackageForTest ].
	base compile: 'baseValue ^ 1'.
	composed := self class classInstaller make: [ :builder |
		builder
			beTrait;
			name: #TRBFTComposed;
			traitComposition: base;
			package: #RefactoringSmallPackageForTest ].
	refactoring := self createRefactoringWithArguments: {
		#RefactoringSmallPackageForTest.
		#RefactoringCOPYSmallPackageForTest }.
	(refactoring classMappings: {
		#RBFTTest1 -> #BarFTTest1.
		#RBFTTest2 -> #BarFTTest2.
		#TRBFTBase -> #TRBFTCopyBase.
		#TRBFTComposed -> #TRBFTCopyComposed } asDictionary) execute.
	copiedBase := environment at: #TRBFTCopyBase.
	copiedComposed := environment at: #TRBFTCopyComposed.
	self assert: copiedComposed isTrait.
	self assert: copiedComposed traitComposition traits asArray equals: { copiedBase }.
	self
		assert: copiedComposed class basicTraitComposition
		equals: copiedComposed traitComposition classComposition.
	self assert: composed traitComposition traits asArray equals: { base }
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageRemapsCopiedTraitDependencies [

	| refactoring |
	refactoring := self createRefactoringWithArguments: {
		TReComposedTraitForDuplication packageName.
		#RefactoringCOPYSmallPackageForTest }.
	self executeRefactoring: (refactoring classMappings: {
		#TReComposedTraitForDuplication -> #TReDuplicatedComposedTrait.
		#TReTraitToBeUsedInDuplication -> #TReDuplicatedTrait } asDictionary).
	self assertCollection: ((refactoring model classNamed: #TReDuplicatedComposedTrait)
		traitComposition traits collect: #name)
		hasSameElements: #( TReDuplicatedTrait ).
	self
		assertCollection: (refactoring model classNamed: #TReComposedTraitForDuplication)
			traitComposition traits
		hasSameElements: { TReTraitToBeUsedInDuplication }
]

{ #category : 'tests' }
RBCopyPackageParametrizedTest >> testCopyPackageWithComposedTraitPreservesBothMethodSides [

	| duplicate refactoring |
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
	self assert: (duplicate classSide directlyDefinesMethod: #classSideValue)
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
