Class {
	#name : 'RBClassTest',
	#superclass : 'RBRefactoringBrowserTest',
	#instVars : [
		'rbNamespace',
		'objectClass',
		'newClass',
		'messageNodeClass'
	],
	#category : 'Refactoring-Core-Tests',
	#package : 'Refactoring-Core-Tests'
}

{ #category : 'running' }
RBClassTest >> setUp [

	super setUp.
	rbNamespace := RBNamespace new.
	objectClass := rbNamespace classNamed: #Object.
	messageNodeClass := rbNamespace classNamed: #RBMessageNode.
	rbNamespace defineClass: [ :aBuilder |
		aBuilder
			name: #SomeClassName;
			superclass: Object;
			slots: { #instanceVariable1. #instanceVariable2 };
			sharedVariables: { #ClassVariable1 };
			sharedPools: 'TextConstants';
			package: #'Refactory-Testing' ].
	newClass := rbNamespace classNamed: #SomeClassName
]

{ #category : 'tests - definition' }
RBClassTest >> testBindingOf [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {Delay . DelayWaitTimeout }).
	root := rbNamespace classNamed: #Delay.
	sub := rbNamespace classNamed: #DelayWaitTimeout.
	
	self assert: (root bindingOf: #Scheduler) equals: (Delay bindingOf: #Scheduler).
	self assert: (sub bindingOf: #Scheduler) equals: (DelayWaitTimeout bindingOf: #Scheduler).
	
	self assert: (root classSide bindingOf: #Scheduler) equals: (Delay class bindingOf: #Scheduler).
	self assert: (sub classSide bindingOf: #Scheduler) equals: (DelayWaitTimeout class bindingOf: #Scheduler).
]

{ #category : 'tests - navigation' }
RBClassTest >> testClassSide [

	| meta |
	meta := objectClass classSide.
	self assert: meta isMeta.
	self assert: meta instanceSide equals: objectClass.
	self deny: objectClass isMeta.
]

{ #category : 'tests - definition' }
RBClassTest >> testDefinesClassVariable [
	self deny: (objectClass definesClassVariable: #ClassVariable1).
	self assert: (objectClass definesClassVariable: self objectClassVariable).
	self assert: (newClass definesClassVariable: #ClassVariable1).
	self deny: (messageNodeClass definesClassVariable: #ClassVariable1).
	self
		assert: (messageNodeClass definesClassVariable: self objectClassVariable)
]

{ #category : 'tests - definition' }
RBClassTest >> testDefinesInstanceVariable [
	self deny: (objectClass definesInstanceVariable: 'instanceVariable1').
	self assert: (newClass definesInstanceVariable: 'instanceVariable1').
	self deny: (messageNodeClass definesInstanceVariable: 'instanceVariable1').
	self assert: (messageNodeClass definesInstanceVariable: 'parent').
	self assert: (messageNodeClass definesInstanceVariable: 'selector')
]

{ #category : 'tests - definition' }
RBClassTest >> testDefinesMethod [
	self assert: (objectClass definesMethod: #printString).
	self assert: (newClass definesMethod: #printString).
	self assert: (messageNodeClass definesMethod: #printString)
]

{ #category : 'tests - definition' }
RBClassTest >> testDefinesPoolDictionary [
	self deny: (objectClass definesPoolDictionary: #OpcodePool).
	self assert: (newClass definesPoolDictionary: #TextConstants).
	self deny: (messageNodeClass definesPoolDictionary: #OpcodePool).
	self assert: ((RBNamespace new classNamed: #Text)
				definesPoolDictionary: #TextConstants)
]

{ #category : 'tests - definition' }
RBClassTest >> testDefinesSharedVariable2 [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState }).
		
	root := rbNamespace classNamed: #MyClassARoot.
	sub := rbNamespace classNamed: #MySubAccessingSuperclassState.
	self assert: (root subclasses includes: sub).
	self assert: (root definesClassVariable: #Shared2).
	
	"a subclass is considered defining a variable even if it is defined by the superclass."
	self assert: (sub definesClassVariable: #Shared2).
]

{ #category : 'tests - definition' }
RBClassTest >> testDefinesTraitMethod [
	| user trait |
	user := rbNamespace classNamed: #MOPTestClassD.
	self assert: (user definesMethod: #c3).
	self assert: (user definesMethod: #c).
	self assert: (user definesMethod: #c2).
	self assert: (user methodFor: #c2) modelClass ~= user.
	self assert: (user methodFor: #c) modelClass ~= user.
	self assert: (user methodFor: #c3) isNil. "we use nil to represent alias"
	trait := rbNamespace classNamed: #Trait2.
	self assert: (user methodFor: #c2) modelClass equals: trait.
	self assert: (user methodFor: #c) modelClass equals: trait
]

{ #category : 'tests - definition' }
RBClassTest >> testDirectlyDefinesSharedVariable [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState }).
		
	root := rbNamespace classNamed: #MyClassARoot.
	sub := rbNamespace classNamed: #MySubAccessingSuperclassState.
	self assert: (root subclasses includes: sub).
	self assert: (root directlyDefinesClassVariable: #Shared2).
	
	self assert: (sub definesClassVariable: #Shared2).
	self deny: (sub directlyDefinesClassVariable: #Shared2).
]

{ #category : 'tests - definition' }
RBClassTest >> testDirectlyDefinesSharedVariable2 [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState }).
		
	root := rbNamespace classNamed: #MyClassARoot.
	sub := rbNamespace classNamed: #MySubAccessingSuperclassState.
	self assert: (root subclasses includes: sub).
	self assert: (root definesClassVariable: #Shared2).
	
	"a subclass is considered defining a variable even if it is defined by the superclass."
	self assert: (sub definesClassVariable: #Shared2).
]

{ #category : 'method tests' }
RBClassTest >> testHierarchy [
	| meta |
	meta := objectClass classSide.
	self assert: (objectClass withAllSubclasses includes: meta).
	self assert: (meta withAllSuperclasses includes: objectClass)
]

{ #category : 'tests - navigation' }
RBClassTest >> testIsEmptyClass [
	
	self deny: objectClass isEmptyClass.
	self assert: (rbNamespace classNamed: #MyClassB) isEmptyClass
]

{ #category : 'tests - navigation' }
RBClassTest >> testIsInstanceSide [

	self deny: objectClass instanceSide isMeta.
	self assert: objectClass classSide isMeta.
	self deny: objectClass classSide instanceSide isMeta.
	
]

{ #category : 'tests - navigation' }
RBClassTest >> testIsMeta [

	self assert: objectClass classSide isMeta.
	self deny: objectClass isMeta
]

{ #category : 'tests' }
RBClassTest >> testObjectIsNotAbstract [
	self deny: objectClass isAbstract.
	self deny: objectClass classSide isAbstract
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhichClassIncludesSelectorDefinedInTheClass [

	self
		assert: (objectClass whichClassIncludesSelector: #'~~>')
		equals: objectClass
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhichClassIncludesSelectorDoNotFind [

	self
		assert: (objectClass whichClassIncludesSelector: #doesNotExistAtAll)
		equals: nil
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhichClassIncludesSelectorDoesNotExist [

	self
		assert: (objectClass whichClassIncludesSelector: #doesNotExistAtAll)
		equals: nil
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhichClassIncludesSelectorGoingUp [

	self
		assert:
			((rbNamespace classNamed: #Model) whichClassIncludesSelector:
				 #'~~>')
		equals: objectClass
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhichClassIncludesSelectorGoingUpAndDoNotFind [

	self
		assert:
			((rbNamespace classNamed: #Model) whichClassIncludesSelector:
				 #doesNotExistAtAll)
		equals: nil
]

{ #category : 'tests - definition' }
RBClassTest >> testWhichMethodsReferToClassVariable [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState }).
		
	root := rbNamespace classNamed: #MyClassARoot.
	sub := rbNamespace classNamed: #MySubAccessingSuperclassState.
	
	self assert: (root whichMethodsReferToSharedVariable:  #Shared1) isNotEmpty.
	self assert: (sub whichMethodsReferToSharedVariable: #Shared1) isNotEmpty.
	
	self assert: (root classSide whichMethodsReferToSharedVariable: #Shared1) isNotEmpty.
	self assert: (sub classSide whichMethodsReferToSharedVariable: #Shared1) isEmpty.
	self assert: (sub classSide whichMethodsReferToSharedVariable: #Shared2) isNotEmpty.
	
]

{ #category : 'tests - definition' }
RBClassTest >> testWhichSelectorsReferToClassVariable [

	| root sub |
	rbNamespace := RBNamespace onEnvironment: 	(RBClassEnvironment classes: {MyClassARoot . MySubAccessingSuperclassState }).
		
	root := rbNamespace classNamed: #MyClassARoot.
	sub := rbNamespace classNamed: #MySubAccessingSuperclassState.
	
	self assert: (root whichSelectorsReferToSharedVariable:  #Shared1) isNotEmpty.
	self assert: (sub whichSelectorsReferToSharedVariable: #Shared1) isNotEmpty.
	
	self assert: (root classSide whichSelectorsReferToSharedVariable: #Shared1) isNotEmpty.
	self assert: (sub classSide whichSelectorsReferToSharedVariable: #Shared1) isEmpty.
	self assert: (sub classSide whichSelectorsReferToSharedVariable: #Shared2) isNotEmpty.
	
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhoDefinesMethodDefinedInTheClass [

	self
		assert: (objectClass whoDefinesMethod: #'~~>')
		equals: objectClass
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhoDefinesMethodDoesNotExist [

	self
		assert: (objectClass whoDefinesMethod: #doesNotExistAtAll)
		equals: nil
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhoDefinesMethodGoingUp [

	self
		assert:
		((rbNamespace classNamed: #Model) whoDefinesMethod: #'~~>')
		equals: objectClass
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhoDefinesMethodGoingUpAndDoNotFind [

	self
		assert:
			((rbNamespace classNamed: #Model) whoDefinesMethod:
				 #doesNotExistAtAll)
		equals: nil
]

{ #category : 'tests - defining methods' }
RBClassTest >> testWhoDefinesMethodSelectorDoNotFind [

	self
		assert: (objectClass whoDefinesMethod: #doesNotExistAtAll)
		equals: nil
]
