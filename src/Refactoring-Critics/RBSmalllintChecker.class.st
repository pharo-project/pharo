"
I apply a lint rule to all classes and methods in a given environment.

My main API is 

- ==run== which resets and runs the rules.
"
Class {
	#name : #RBSmalllintChecker,
	#superclass : #Object,
	#instVars : [
		'rule',
		'environment',
		'context',
		'methodBlock'
	],
	#category : #'Refactoring-Critics'
}

{ #category : #'instance creation' }
RBSmalllintChecker class >> newWithContext [
	^(self new)
		context: RBSmalllintContext new;
		yourself
]

{ #category : #'instance creation' }
RBSmalllintChecker class >> runRule: aLintRule [
	(self new)
		rule: aLintRule;
		run.
	^aLintRule
]

{ #category : #'instance creation' }
RBSmalllintChecker class >> runRule: aLintRule onEnvironment: aBrowserEnvironment [
	aLintRule resetResult.
	(self new)
		rule: aLintRule;
		environment: aBrowserEnvironment;
		run.
	^aLintRule
]

{ #category : #private }
RBSmalllintChecker >> checkClass: aClass [ 
	(environment includesClass: aClass) ifTrue: [rule checkClass: aClass]
]

{ #category : #private }
RBSmalllintChecker >> checkMethodsForClass: aClass [
	^ environment
		selectorsForClass: aClass
		do: [ :each | 
			rule checkMethod: aClass>>each.
			methodBlock value ]
]

{ #category : #private }
RBSmalllintChecker >> checkPackage: aRPackage [
	(environment includesPackage: aRPackage) 
		ifTrue: [rule checkPackage: aRPackage]
]

{ #category : #accessing }
RBSmalllintChecker >> context: aSmalllintContext [ 
	context := aSmalllintContext
]

{ #category : #accessing }
RBSmalllintChecker >> environment: aBrowserEnvironment [ 
	environment := aBrowserEnvironment
]

{ #category : #initialization }
RBSmalllintChecker >> initialize [
	methodBlock := [].
	environment := RBBrowserEnvironment new.
	context := RBSmalllintContext newNoCache
]

{ #category : #accessing }
RBSmalllintChecker >> methodBlock: aBlock [ 
	methodBlock := aBlock
]

{ #category : #initialization }
RBSmalllintChecker >> release [
	context release.
	super release
]

{ #category : #accessing }
RBSmalllintChecker >> rule: aLintRule [ 
	rule := aLintRule
]

{ #category : #actions }
RBSmalllintChecker >> run [
	environment classesAndTraits
		do: [ :class |
			self checkClass: class.
			self checkMethodsForClass: class.
			
			self checkClass: class classSide.
			self checkMethodsForClass: class classSide ]
		displayingProgress: [ :class | 'Running critics on: ', class name ].
	environment packages 
		do: [ :package | self checkPackage: package ]
		displayingProgress: [ :package | 'Running critics on: ', package name ].
]
