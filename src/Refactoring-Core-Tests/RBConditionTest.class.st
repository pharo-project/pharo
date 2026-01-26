Class {
	#name : 'RBConditionTest',
	#superclass : 'TestCase',
	#instVars : [
		'rbNamespace',
		'objectClass',
		'messageNodeClass',
		'newClass'
	],
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'running' }
RBConditionTest >> setUp [
	| classenv classes |
	super setUp.
	classes := { Object . OCMessageNode . TextConstants } asOrderedCollection addAll: self class withAllSuperclasses; yourself.
	classenv := RBClassEnvironment classes: classes.
	rbNamespace := RBNamespace onEnvironment: classenv.
	
	objectClass := rbNamespace classNamed: #Object.
	messageNodeClass := rbNamespace classNamed: #OCMessageNode.
	rbNamespace defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #SomeClassName;
			slots: { #instanceVariable1. #instanceVariable2 };
			sharedVariables: { #ClassVariable1 };
			sharedPools: { #TextConstants };
			package: #'Refactory-Testing' ].
	newClass := rbNamespace classNamed: #SomeClassName
]

{ #category : 'tests' }
RBConditionTest >> testCheckFailEarlyAndDoesNotCoverEveryConditions [

	| falseCase trueCase composition |
	falseCase := RBCondition
		             withBlock: [ false ]
		             errorString: 'The false case'.
	trueCase := RBCondition
		            withBlock: [ false ]
		            errorString: 'The second false case'.
	composition := falseCase & trueCase.
	self deny: composition check.
	self
		assert: composition errorString
		equals: 'The false case'
]

{ #category : 'tests' }
RBConditionTest >> testCheckInvalidMethodName [
	"Usually used to validate input."

	self deny: (RBCondition isValidSelector: 'fofo fo') check.
	self deny: (RBCondition isValidSelector: '123fofo') check.
	self deny: (RBCondition isValidSelector: 'foo::') check.
	self deny: (RBCondition isValidSelector: 'agr:goo:aa') check.
	self deny: (RBCondition isValidSelector: 'foo:123:') check.
	self deny: (RBCondition isValidSelector: 'foo[arg]') check.
	self deny: (RBCondition isValidSelector: 'foo:=arg') check.
	self deny: (RBCondition isValidSelector: 'foo:arg)') check.
	self deny: (RBCondition isValidSelector: 'foo:(arg)') check.
	self deny: (RBCondition isValidSelector: 'foo:+arg)') check.
	self deny: (RBCondition isValidSelector: '<<foo:<<arg)') check.
	self deny: (RBCondition isValidSelector: 'foo:agr^:') check.
	self deny: (RBCondition isValidSelector: 'foo:agr') check
]

{ #category : 'tests' }
RBConditionTest >> testCheckThatAndIsWorking [

	| falseCase trueCase composition |
	falseCase := RBCondition
		             withBlock: [ false ]
		             errorString: 'The false case'.
	trueCase := RBCondition
		            withBlock: [ true ]
		            errorString: 'The true case'.
	composition := falseCase & trueCase.
	self deny: composition check
]

{ #category : 'tests' }
RBConditionTest >> testCheckThatOnlyFailingConditionErrorIsReported [

	| falseCase trueCase composition |
	falseCase := RBCondition
		             withBlock: [ false ]
		             errorString: 'The false case'.
	trueCase := RBCondition
		            withBlock: [ true ]
		            errorString: 'The true case'.
	composition := falseCase & trueCase.
	self deny: composition check.
	self assert: composition errorString equals: 'The false case'
]

{ #category : 'tests' }
RBConditionTest >> testCheckThatOnlyFailingConditionErrorIsReportedTrueCaseFirst [

	| falseCase trueCase composition |
	trueCase := RBCondition
		             withBlock: [ true ]
		             errorString: 'The true case'.
	falseCase := RBCondition
		            withBlock: [ false ]
		            errorString: 'The false case'.
	composition := trueCase & falseCase.
	self deny: composition check.
	self
		assert: composition errorString
		equals: 'The false case'
]

{ #category : 'tests' }
RBConditionTest >> testCheckValidMethodName [
	"Usually used to validate input."
	
	self assert: (RBCondition isValidSelector: 'foo') check.
	self assert: (RBCondition isValidSelector: #foo) check.
	self assert: (RBCondition isValidSelector: #+) check
]

{ #category : 'tests - references' }
RBConditionTest >> testHierarchyOfReferencesInstanceVariable [

	| testClass |
	testClass := rbNamespace classNamed: self class name.
	self assert: (testClass withAllSuperclasses includes: objectClass).
	self assert: (RBCondition
			 hierarchyOf: testClass
			 referencesInstanceVariable: #objectClass) check
]

{ #category : 'tests - references' }
RBConditionTest >> testHierarchyOfReferencesInstanceVariable2 [

	| testClass |
	testClass := rbNamespace classNamed: self class name.
	
	self assert: (testClass withAllSuperclasses includes: objectClass).
	"the class itself refers to its own objectClass instance variable"
	self assert: (ReInstanceVariableHasReferences new
			 				hierarchyOf: testClass
			 				referencesInstanceVariable: #objectClass) check
]

{ #category : 'tests - references' }
RBConditionTest >> testHierarchyOfReferencesSharedVariableAccessing [

	| textConstantClass |
	textConstantClass := rbNamespace classNamed: #TextConstants.
	"only the class side is accessing the shared variable. But the query should 
	check both sides."

	self assert: (RBCondition
			 hierarchyOf: textConstantClass
			 referencesSharedVariable: #Basal) check.

	self assert: (RBCondition
			 hierarchyOf: textConstantClass classSide
			 referencesSharedVariable: #Basal) check
]

{ #category : 'tests - references' }
RBConditionTest >> testHierarchyOfReferencesSharedVariableFromClassSide [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState }).
		
	root := rbNamespace classNamed: #MyClassARoot.
	sub := rbNamespace classNamed: #MySubAccessingSuperclassState.
	self assert: (root subclasses includes: sub).
	self assert: (root definesClassVariable: #Shared2).
	self assert: (sub definesClassVariable: #Shared2).
							
	self assert: (ReSharedVariableHasReferences new
			 				hierarchyOf: root
			 				referencesSharedVariable: #SharedOnlyInClassSideSubclass) check.
	self assert: (ReSharedVariableHasReferences new
			 				hierarchyOf: root classSide
			 				referencesSharedVariable: #SharedOnlyInClassSideSubclass) check.
]

{ #category : 'tests - references' }
RBConditionTest >> testHierarchyOfReferencesSharedVariableFromSubclass [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState }).
		
	root := rbNamespace classNamed: #MyClassARoot.
	sub := rbNamespace classNamed: #MySubAccessingSuperclassState.
	self assert: (root subclasses includes: sub).
	self assert: (root definesClassVariable: #Shared2).

	"We access the class var from the superclass only from a subclass."	
	self assert: (ReSharedVariableHasReferences new
			 				hierarchyOf: root
			 				referencesSharedVariable: #Shared2) check.

]

{ #category : 'tests - references' }
RBConditionTest >> testHierarchyOfReferencesSharedVariableInDefiningClass [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState }).
		
	root := rbNamespace classNamed: #MyClassARoot.
	sub := rbNamespace classNamed: #MySubAccessingSuperclassState.
	self assert: (root subclasses includes: sub).
	
	self assert: (ReSharedVariableHasReferences new
			 				hierarchyOf: root
			 				referencesSharedVariable: #Shared1) check
]

{ #category : 'tests - references' }
RBConditionTest >> testHierarchyOfReferencesSharedVariableNotAccessing [

	| testClass |
	testClass := rbNamespace classNamed: self class name.

	self deny: (RBCondition
			 hierarchyOf: newClass
			 referencesSharedVariable: #ClassVariable1) check
]

{ #category : 'tests' }
RBConditionTest >> testInvalidArgumentsForSelector [

	| aModel aClass |
	aModel := RBNamespace new.
	aModel defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #FOOBAR;
			slots: { #asdf };
			package: #'Refactory-Test data' ].
	aClass := aModel classNamed: #FOOBAR.
	aClass compile: 'eiffel: tower ^ self ' classified: 'dummy'.

	self assert: (RBCondition invalidArgumentNamesForSelector: #eiffel: in: aModel) asArray equals: #( #asdf #tower )
]

{ #category : 'tests' }
RBConditionTest >> testIsClassDefinedInModel [

	| aModel aClass |
	aModel := RBNamespace new.
	aModel defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #FOOBAR;
			slots: { #asdf };
			package: #'Refactory-Test data' ].
	aClass := aModel classNamed: #FOOBAR.
	self assert: (RBCondition isClass: aClass definedIn: aModel) check
]

{ #category : 'tests' }
RBConditionTest >> testIsClassNamedDefinedIn [

	| aModel |
	aModel := RBNamespace new.
	aModel defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #FOOBAR;
			slots: { #asdf };
			package: #'Refactory-Test data' ].
	self assert: (RBCondition isClassNamed: #FOOBAR definedIn: aModel) check
]

{ #category : 'tests' }
RBConditionTest >> testIsClassNamedDefinedInNot [

	| aModel |
	aModel := RBNamespace new.
	aModel defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #FOOBAR;
			slots: { #asdf };
			package: #'Refactory-Test data' ].
	self deny: (RBCondition isClassNamed: #FOOBAR2 definedIn: aModel) check
]

{ #category : 'tests' }
RBConditionTest >> testTrue [
	
	self assert: RBCondition true check.
	self deny: RBCondition true not check.
]
