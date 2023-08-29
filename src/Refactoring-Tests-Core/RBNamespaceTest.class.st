Class {
	#name : #RBNamespaceTest,
	#superclass : #RBRefactoringBrowserTest,
	#instVars : [
		'modelFactory'
	],
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBNamespaceTest >> setUp [
	super setUp.
	modelFactory := RBClassModelFactory new
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testAllClassesDo [
	| model classes |
	classes := 0.
	model := modelFactory rbNamespace new.
	model allClassesDo:
			[:each |
			each name = #Object ifTrue: [each allSubclasses].
			classes := classes + 1].
	RBBrowserEnvironment new classesDo: [:each | classes := classes - 1].
	self assert: classes equals: 0
]

{ #category : #'tests - on environment' }
RBNamespaceTest >> testAllImplementorsOnClassSubset [

	| rbclasses rbNamespace2 |
	rbclasses := RBClassEnvironment classes: {
			             RBClassToRename.
			             RBSubclassOfClassToRename }.
	rbNamespace2 := RBNamespace onEnvironment: rbclasses.
	"there are several method #method1 in the system will in this slice we only have 1."
	self
		assert: (rbNamespace2 allImplementorsOf: #method1) size
		equals: 1.

	self
		assert: (rbNamespace2 allImplementorsOf: #printOn:) size
		equals: 1.
	self
		assert:
		(rbNamespace2 allImplementorsOf: #equallyDefinedInSubclass) size
		equals: 2
]

{ #category : #'tests - on environment' }
RBNamespaceTest >> testAllReferencesToClassDo [

	| model packageenv count |
	packageenv := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName.
			           'Refactoring2-Transformations-Tests' }.
	"The second package has references to the leaf subclass we want to remove and this is part of the test."
	model := RBNamespace onEnvironment: packageenv.
	
	count := 0.
	(model allReferencesToClass: (model classFor: RBFooDummyLintRuleTest1) do:[ :each | count := count + 1]).
	self
		assert: count
		equals: 10.	
]

{ #category : #'tests - on environment' }
RBNamespaceTest >> testAllReferencesToClassDoMore [

	| model packageenv selectors |
	packageenv := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName.
			           'Refactoring2-Transformations-Tests' }.
	"The second package has references to the leaf subclass we want to remove and this is part of the test."
	model := RBNamespace onEnvironment: packageenv.
	
	selectors := OrderedCollection new.
	(model allReferencesToClass: (model classFor: RBFooDummyLintRuleTest1) do:[ :each | selectors add: each selector]).

	self
		assert: selectors asArray
		equals: #(#testCanRemoveUnreferencedClass #testCanRemoveReferencedClass #testCanRemoveSuperclass #testRemovingAnEmptyNonLeafClassReparent #testCanRemoveUnreferencedClassUsingLimitedEnvironmentButReferencedElsewhere #testRemoveAClassAndTheirSubclass #testRemovingAnEmptyNonLeafClassIsAllowed #testPreconditionNotEmptyClass #testRemoveAClassAndTheirSubclass2UsingAlimitedEnvironment #testCanRemoveAReferencedClassNotHavingReferenceInTheModel)
]

{ #category : #'tests - on environment' }
RBNamespaceTest >> testAllReferencesToClassDoWithoutReferencesInTheSpecifiedModel [

	| model packageenv count |
	packageenv := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName		            }.

	model := RBNamespace onEnvironment: packageenv.
	
	count := 0.
	(model allReferencesToClass: (model classFor: RBFooDummyLintRuleTest1) do:[ :each | count := count + 1]).
	self
		assert: count
		equals: 0.	
]

{ #category : #'tests - on environment' }
RBNamespaceTest >> testAllReferencesToOnClassSubset [

	| rbclasses rbNamespace2 |
	rbclasses := RBClassEnvironment classes: {
			             RBClassToRename.
			             RBSubclassOfClassToRename }.
	rbNamespace2 := RBNamespace onEnvironment: rbclasses.

	self
		assertEmpty: 
		(rbNamespace2 allReferencesTo: #methodThatIsNotUsedForSure).
		
	"A method just defined the same is not a reference"
	self
		assertEmpty:
		(rbNamespace2 allReferencesTo: #equallyDefinedInSubclass) 
		
]

{ #category : #'tests - on environment' }
RBNamespaceTest >> testAllReferencesToTakesIntoAccountSuperCall [

	| rbclasses rbNamespace2 |
	rbclasses := RBClassEnvironment classes: {
			             RBClassToRename.
			             RBSubclassOfClassToRename }.
	rbNamespace2 := RBNamespace onEnvironment: rbclasses.

	self
		assert:
		(rbNamespace2 allReferencesTo: #justSuperSendInSubclass) size
		equals: 2
]

{ #category : #'tests - on environment' }
RBNamespaceTest >> testClassesReferencingClass [

	| model packageenv classes |
	packageenv := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName.
			           'Refactoring2-Transformations-Tests' }.
	"The second package has references to the leaf subclass we want to remove and this is part of the test."
	model := RBNamespace onEnvironment: packageenv.
	
	classes := model classesReferencingClass: (model classFor: RBFooDummyLintRuleTest1).
	self
		assert: (classes asArray collect: [ :each | each name ]) 
		equals: #(#RBRemoveClassTransformationTest #RBRemoveClassRefactoringTest)
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testCommentChange [
	| st cl |
	st := modelFactory rbNamespace new.
	cl := st classNamed: self class name.
	self assert: cl comment isString.
	cl comment: 'a comment'.
	self assert: cl comment equals: 'a comment'.
	self assert: st changes changes size equals: 1.
	self assert: st changes changes first comment equals: 'a comment'.
	cl comment: nil.
	self assert: cl comment isNil.
	self assert: st changes changes size equals: 2.
	self assert: st changes changes last comment isNil
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testDefineClassAfterDeletedChange [
	| st |
	st := modelFactory rbNamespace new.
	st removeClassNamed: self class name.
	self deny: (st includesClassNamed: self class name).
	st defineClass: self class oldDefinition.
	self assert: (st includesClassNamed: self class name).
	self assert: (st classNamed: self class name) notNil
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testDefineClassChange [
	| st |
	st := modelFactory rbNamespace new.
	st
		defineClass: 'RefactoringBrowserTest << #SmalltalkTestXXX
				package: #''Refactory-Testing'''.
	self assert: (st includesClassNamed: #SmalltalkTestXXX).
	self assert: (st classNamed: #SmalltalkTestXXX) notNil
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testImplementors [
	| st |
	st := modelFactory rbNamespace new.
	self assert: ((st allImplementorsOf: #printString)
				includes: (st classNamed: #Object)).
	(st classNamed: #Object) removeMethod: #printString.
	self deny: ((st allImplementorsOf: #printString)
				includes: (st classNamed: #Object))
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testIncludesClass [
	self assert: (modelFactory rbNamespace new includesClassNamed: #Object).
	self deny: (modelFactory rbNamespace new includesClassNamed: #Object1).
	self
		deny: ((modelFactory rbNamespace
				onEnvironment: (RBClassEnvironment onEnvironment: RBBrowserEnvironment new
						classes: (Array with: Object)))
					includesClassNamed: #OrderedCollection)
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testModelImplementorsSenders [
	| model class modelImps refs found |
	model := modelFactory rbNamespace new.
	model
		defineClass: 'Object << #Asdf package: #''Refactory-Testing'''.
	class := model classNamed: #Asdf.
	class compile: 'storeOn: aStream ^super storeOn: aStream'
		classified: #(#printing).
	modelImps := model allImplementorsOf: #storeOn:.
	self assert: (modelImps includes: class).
	self assert: modelImps size - 1
				equals: (RBBrowserEnvironment new implementorsOf: #storeOn:) numberSelectors.
	refs := RBBrowserEnvironment new referencesTo: #storeOn:.
	found := false.
	model allReferencesTo: #storeOn:
		do:
			[:each |
			each modelClass = class
				ifTrue: [found := true]
				ifFalse:
					[	self
						assert: (refs includesSelector: each selector in: each modelClass realClass)]].
	self assert: found
]

{ #category : #'class tests' }
RBNamespaceTest >> testPopulateModel [

	| newModel |
	newModel := RBNamespace onEnvironment: RBClassEnvironment new.
	
	newModel defineClass: (ClassDefinitionPrinter oldPharo for: MyClassA) classDefinitionString.
	newModel defineClass: (ClassDefinitionPrinter oldPharo for: MyClassB) classDefinitionString.
	newModel defineClass: (ClassDefinitionPrinter oldPharo for: MyClassC) classDefinitionString.
	newModel name: 'Test'.
	
	
	self assert: (newModel classNamed: #MyClassA) name equals: 'MyClassA'.
	self assert: (newModel classNamed: #MyClassB) superclass name equals: 'MyClassA'.
	self assert: (newModel classNamed: #MyClassC) superclass name equals: 'MyClassA'.

]

{ #category : #'tests - classes' }
RBNamespaceTest >> testRedefineClassChange [
	| st |
	st := modelFactory rbNamespace new.
	st defineClass: 'nil << #Object
				slots: { #a};
				sharedVariables: {#A};
				sharedPools: {TextConstants};
				package: #''Refactory-Testing'''.
	self assert: (st includesClassNamed: #Object).
	self assert: (st classNamed: #Object) notNil
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testReferencesPrintOn [
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := modelFactory rbNamespace new.
	smalltalk allReferencesTo: #printOn: do: [ :each |
		hasFoundObject := hasFoundObject
			or: [ each selector = #fullPrintString
			and: [each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #testReferencesPrintOn
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self assert: hasFoundObject.
	self assert: hasFoundSelf
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testReferencesPrintOnAfterAddition [
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := modelFactory rbNamespace new.
	(smalltalk classNamed: #Object)
		compile: 'someTestReference self printOn: nil'
		classified: #(testing).
	(smalltalk classNamed: self class name)
		compile: 'someTestReference ^ #printOn:'
		classified: #(testing).
	smalltalk allReferencesTo: #printOn: do: [ :each |
		hasFoundObject := hasFoundObject
			or: [ each selector = #someTestReference
			and: [ each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #someTestReference
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self assert: hasFoundObject.
	self assert: hasFoundSelf
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testReferencesPrintOnAfterRemove [
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := modelFactory rbNamespace new.
	(smalltalk classNamed: #Object)
		removeMethod: #printString.
	(smalltalk classNamed: self class name)
		removeMethod: #testReferencesPrintOnAfterRemove.
	smalltalk allReferencesTo: #printOn: do: [ :each |
		hasFoundObject := hasFoundObject
			or: [ each selector = #printString
			and: [ each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #testReferencesPrintOnAfterRemove
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self deny: hasFoundObject.
	self deny: hasFoundSelf
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testRemoveClassChange [
	| st |
	st := modelFactory rbNamespace new.
	st removeClassNamed: self class name.
	self deny: (st includesClassNamed: self class name).
	self assert: (st classNamed: self class name) isNil
]

{ #category : #'tests - classes' }
RBNamespaceTest >> testReparentSuperclassChange [
	| st superclass subclasses |
	st := modelFactory rbNamespace new.
	superclass := st classFor: TestCase superclass.
	subclasses := TestCase subclasses collect: [:each | st classFor: each].
	st reparentClasses: subclasses to: superclass.
	subclasses do: [:each | self assert: each superclass equals: superclass]
]

{ #category : #'class tests' }
RBNamespaceTest >> testRoots [

	| model |
	model := modelFactory rbNamespace new.
	self
		assert:
			(model rootClasses asSortedCollection: [ :a :b | a name < b name ])
				asArray
		equals:
			(({ ProtoObject} collect: [ :each | model classFor: each ])
				 asSortedCollection: [ :a :b | a name < b name ]) asArray
]
