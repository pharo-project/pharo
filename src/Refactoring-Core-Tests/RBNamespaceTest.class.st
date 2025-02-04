Class {
	#name : 'RBNamespaceTest',
	#superclass : 'RBRefactoringBrowserTest',
	#instVars : [
		'namespace'
	],
	#category : 'Refactoring-Core-Tests-Model',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Model'
}

{ #category : 'running' }
RBNamespaceTest >> setUp [
	super setUp.
	namespace := RBNamespace new
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testAllClassesDo [
	| classes |
	classes := 0.
	namespace allClassesDo: [:each | classes := classes + 1].
	RBBrowserEnvironment new classesDo: [:each | classes := classes - 1].
	self assert: classes equals: 0
]

{ #category : 'tests - on environment' }
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

{ #category : 'tests - on environment' }
RBNamespaceTest >> testAllReferencesToClassDo [

	| model packageenv count |
	packageenv := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName.
			           'Refactoring-Transformations-Tests' }.
	"The second package has references to the leaf subclass we want to remove and this is part of the test."
	model := RBNamespace onEnvironment: packageenv.
	
	count := 0.
	(model allReferencesToClass: (model classFor: RBFooDummyLintRuleTest1) 
		do:[ :each | count := count + 1]).
	self
		assert: count
		equals: 10.	
]

{ #category : 'tests - on environment' }
RBNamespaceTest >> testAllReferencesToClassDoMore [

	| model packageenv selectors |
	packageenv := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName.
			           'Refactoring-Transformations-Tests' }.
	"The second package has references to the leaf subclass we want to remove and this is part of the test."
	model := RBNamespace onEnvironment: packageenv.
	
	selectors := OrderedCollection new.
	(model allReferencesToClass: (model classFor: RBFooDummyLintRuleTest1) do:[ :each | selectors add: each selector]).

	self
		assertCollection: selectors asArray
		hasSameElements: #(#testCanRemoveUnreferencedClass #testCanRemoveReferencedClass #testCanRemoveSuperclass #testRemovingAnEmptyNonLeafClassReparent #testCanRemoveUnreferencedClassUsingLimitedEnvironmentButReferencedElsewhere #testRemoveAClassAndTheirSubclass #testRemovingAnNonEmptyLeafClassIsAllowed #testPreconditionNotEmptyClass #testRemoveAClassAndTheirSubclass2UsingAlimitedEnvironment #testCanRemoveAReferencedClassNotHavingReferenceInTheModel)
]

{ #category : 'tests - on environment' }
RBNamespaceTest >> testAllReferencesToClassDoWithoutReferencesInTheSpecifiedModel [

	| model packageenv count |
	packageenv := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName		            }.

	model := RBNamespace onEnvironment: packageenv.
	
	count := 0.
	(model allReferencesToClass: (model classFor: RBFooDummyLintRuleTest1) 
		do:[ :each | count := count + 1]).
	self
		assert: count
		equals: 0.	
]

{ #category : 'tests - on environment' }
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

{ #category : 'tests - on environment' }
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

{ #category : 'tests - on environment' }
RBNamespaceTest >> testClassesReferencingClass [

	| model packageenv classes |
	packageenv := RBPackageEnvironment packageNames: {
			           RBFooDummyLintRuleTest1 packageName.
			           'Refactoring-Transformations-Tests' }.
	"The second package has references to the leaf subclass we want to remove and this is part of the test."
	model := RBNamespace onEnvironment: packageenv.
	
	classes := model classesReferencingClass: (model classFor: RBFooDummyLintRuleTest1).
	self
		assertCollection: (classes asArray collect: [ :each | each name ]) 
		hasSameElements: #(#RBRemoveClassTransformationTest #RBRemoveClassRefactoringTest)
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testCommentChange [
	| cl classenv |
	classenv := RBClassEnvironment class: self class.
	namespace := RBNamespace onEnvironment: classenv.
	
	cl := namespace classNamed: self class name.
	self assert: cl comment isString.
	cl comment: 'a comment'.
	self assert: cl comment equals: 'a comment'.
	self assert: namespace changes changes size equals: 1.
	self assert: namespace changes changes first comment equals: 'a comment'.
	cl comment: nil.
	self assert: cl comment isNil.
	self assert: namespace changes changes size equals: 2.
	self assert: namespace changes changes last comment isNil
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testDefineClassAfterDeletedChange [
	| packageenv |
	packageenv := RBPackageEnvironment packageNames: { self class packageName }.
	namespace := RBNamespace onEnvironment: packageenv.
	
	namespace removeClassNamed: self class name.
	self deny: (namespace includesClassNamed: self class name).
	namespace defineClass: [ :aBuilder | aBuilder fillFor: self class ].
	self assert: (namespace includesClassNamed: self class name).
	self assert: (namespace classNamed: self class name) isNotNil
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testDefineClassChange [
	| classenv |
	classenv := RBClassEnvironment classes: {}.
	namespace := RBNamespace onEnvironment: classenv.
	
	namespace defineClass: [ :aBuilder |
		aBuilder
			superclassName: #RefactoringBrowserTest;
			name: #SmalltalkTestXXX;
			package: #'Refactory-Testing' ].
	self assert: (namespace includesClassNamed: #SmalltalkTestXXX).
	self assert: (namespace classNamed: #SmalltalkTestXXX) isNotNil
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testImplementors [

	self assert: ((namespace allImplementorsOf: #printString)
				includes: (namespace classNamed: #Object)).
	(namespace classNamed: #Object) removeMethod: #printString.
	self deny: ((namespace allImplementorsOf: #printString)
				includes: (namespace classNamed: #Object))
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testIncludesClass [
	self assert: (namespace includesClassNamed: #Object).
	self deny: (namespace includesClassNamed: #Object1).
	self
		deny: ((RBNamespace
				onEnvironment: (RBClassEnvironment onEnvironment: RBBrowserEnvironment new
						classes: (Array with: Object)))
					includesClassNamed: #OrderedCollection)
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testModelImplementorsSenders [

	| class modelImps refs found |
	namespace defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #Asdf;
			package: #'Refactory-Testing' ].
	class := namespace classNamed: #Asdf.
	class compile: 'storeOn: aStream ^super storeOn: aStream' classified: 'printing'.
	modelImps := namespace allImplementorsOf: #storeOn:.
	self assert: (modelImps includes: class).
	self assert: modelImps size - 1 equals: (RBBrowserEnvironment new implementorsOf: #storeOn:) numberSelectors.
	refs := RBBrowserEnvironment new referencesTo: #storeOn:.
	found := false.
	namespace allReferencesTo: #storeOn: do: [ :each |
		each modelClass = class
			ifTrue: [ found := true ]
			ifFalse: [ self assert: (refs includesSelector: each selector in: each modelClass realClass) ] ].
	self assert: found
]

{ #category : 'class tests' }
RBNamespaceTest >> testPopulateModel [

	| newModel |
	newModel := RBNamespace onEnvironment: RBClassEnvironment new.

	newModel defineClass: [ :aBuilder | aBuilder fillFor: MyClassA ].
	newModel defineClass: [ :aBuilder | aBuilder fillFor: MyClassB ].
	newModel defineClass: [ :aBuilder | aBuilder fillFor: MyClassC ].
	newModel name: 'Test'.


	self assert: (newModel classNamed: #MyClassA) name equals: 'MyClassA'.
	self assert: (newModel classNamed: #MyClassB) superclass name equals: 'MyClassA'.
	self assert: (newModel classNamed: #MyClassC) superclass name equals: 'MyClassA'
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testRedefineClassChange [

	namespace defineClass: [ :aBuilder |
		aBuilder
			superclass: nil;
			name: #RBNamespaceTest;
			slots: { #a };
			sharedVariables: { #A };
			sharedPools: #(TextConstants);
			package: #'Refactory-Testing' ].
	self assert: (namespace includesClassNamed: #Object).
	self assert: (namespace classNamed: #Object) isNotNil
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testReferencesPrintOn [

	| hasFoundObject hasFoundSelf  |
	hasFoundObject := false.
	hasFoundSelf := false.
	namespace allReferencesTo: #printOn: do: [ :each |
		hasFoundObject := hasFoundObject
			or: [ each selector = #fullPrintString
			and: [each modelClass = (namespace classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #testReferencesPrintOn
			and: [ each modelClass = (namespace classNamed: self class name) ] ] ].
	self assert: hasFoundObject.
	self assert: hasFoundSelf
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testReferencesPrintOnAfterAddition [

	| classenv hasFoundObject hasFoundSelf |
	classenv := RBClassEnvironment classes: { Object . self class }.
	namespace := RBNamespace onEnvironment: classenv.
	
	hasFoundObject := false.
	hasFoundSelf := false.
	
	(namespace classNamed: #Object)
		compile: 'someTestReference self printOn: nil'
		classified: 'testing'.
	(namespace classNamed: self class name)
		compile: 'someTestReference ^ #printOn:'
		classified: 'testing'.
	namespace allReferencesTo: #printOn: do: [ :each |
		hasFoundObject := hasFoundObject
			or: [ each selector = #someTestReference
			and: [ each modelClass = (namespace classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #someTestReference
			and: [ each modelClass = (namespace classNamed: self class name) ] ] ].
	self assert: hasFoundObject.
	self assert: hasFoundSelf
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testReferencesPrintOnAfterRemove [

	| classenv hasFoundObject hasFoundSelf |
	classenv := RBClassEnvironment classes: { Object . self class }.
	namespace := RBNamespace onEnvironment: classenv.
	
	hasFoundObject := false.
	hasFoundSelf := false.

	(namespace classNamed: #Object)
		removeMethod: #printString.
	(namespace classNamed: self class name)
		removeMethod: #testReferencesPrintOnAfterRemove.
	namespace allReferencesTo: #printOn: do: [ :each |
		hasFoundObject := hasFoundObject
			or: [ each selector = #printString
			and: [ each modelClass = (namespace classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #testReferencesPrintOnAfterRemove
			and: [ each modelClass = (namespace classNamed: self class name) ] ] ].
	self deny: hasFoundObject.
	self deny: hasFoundSelf
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testRemoveClassChange [
	| classenv |
	classenv := RBClassEnvironment classes: { self class }.
	namespace := RBNamespace onEnvironment: classenv.
	
	namespace removeClassNamed: self class name.
	self deny: (namespace includesClassNamed: self class name).
	self assert: (namespace classNamed: self class name) isNil
]

{ #category : 'tests - classes' }
RBNamespaceTest >> testReparentSuperclassChange [

	| classes classenv superclass subclasses |
	classes := TestCase subclasses asOrderedCollection addAll: { TestCase . TestCase superclass }; yourself.
	classenv := RBClassEnvironment classes: classes.
	namespace := RBNamespace onEnvironment: classenv.
	
	superclass := namespace classFor: TestCase superclass.
	subclasses := TestCase subclasses collect: [:each | namespace classFor: each].
	namespace reparentClasses: subclasses to: superclass.
	subclasses do: [:each | self assert: each superclass equals: superclass]
]

{ #category : 'class tests' }
RBNamespaceTest >> testRoots [

	self
		assert:
			(namespace rootClasses asSortedCollection: [ :a :b | a name < b name ])
				asArray
		equals:
			(({ ProtoObject} collect: [ :each | namespace classFor: each ])
				 asSortedCollection: [ :a :b | a name < b name ]) asArray
]
