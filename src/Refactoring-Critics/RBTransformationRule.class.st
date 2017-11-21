"
A RBTransformationRule is a special rule that not only detects problems but also can automatically transform the good.
"
Class {
	#name : #RBTransformationRule,
	#superclass : #RBLintRule,
	#instVars : [
		'rewriteRule',
		'builder',
		'class'
	],
	#classVars : [
		'RecursiveSelfRule'
	],
	#category : #Refactoring-Critics
}

{ #category : #testing }
RBTransformationRule class >> checksMethod [
	^ true
]

{ #category : #cleanup }
RBTransformationRule class >> cleanUp [
	RecursiveSelfRule := nil.
]

{ #category : #initialization }
RBTransformationRule class >> initialize [
	self initializeRecursiveSelfRule
]

{ #category : #initialization }
RBTransformationRule class >> initializeRecursiveSelfRule [
	RecursiveSelfRule := RBParseTreeSearcher new.
	RecursiveSelfRule 
		matchesAnyMethodOf: #(
			'`@methodName: `@args | `@temps | self `@methodName: `@args1' 
			'`@methodName: `@args | `@temps | ^ self `@methodName: `@args1')
		do: [ :node :answer | true ].
	^ RecursiveSelfRule
]

{ #category : #testing }
RBTransformationRule class >> isVisible [
	^ self name ~= #RBTransformationRule
]

{ #category : #accessing }
RBTransformationRule class >> recursiveSelfRule [
	^ RecursiveSelfRule ifNil: [ self initializeRecursiveSelfRule ]
]

{ #category : #accessing }
RBTransformationRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'TransformationRule'
]

{ #category : #running }
RBTransformationRule >> basicCheck: aMethod [ 
	aMethod isFromTrait
		ifTrue: [ ^ false ].
	(self rewriteRule canMatchMethod: aMethod) 
		ifFalse: [ ^ false ].
	class := aMethod methodClass.
	^ (self rewriteRule executeTree: aMethod ast copy) and: [
	  (self class recursiveSelfRule
			executeTree: rewriteRule tree
			initialAnswer: false) not ]
]

{ #category : #accessing }
RBTransformationRule >> builder [
	^ builder
		ifNil: [ 
			self resetResult.
			builder ]
]

{ #category : #accessing }
RBTransformationRule >> changes [
	^ self builder changes
]

{ #category : #running }
RBTransformationRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: [
		self builder compile: rewriteRule tree newSource in: class classified: aMethod protocol ]
]

{ #category : #testing }
RBTransformationRule >> hasConflicts [
	^ true
]

{ #category : #initialization }
RBTransformationRule >> initialize [
	super initialize.
	rewriteRule := RBParseTreeRewriter new
]

{ #category : #testing }
RBTransformationRule >> isEmpty [
	^ self builder changes isEmpty
]

{ #category : #testing }
RBTransformationRule >> isTransformationRule [

	^ true 
]

{ #category : #accessing }
RBTransformationRule >> name [

	^ 'Code Rewriting'
]

{ #category : #accessing }
RBTransformationRule >> problemCount [
	^ self builder problemCount
]

{ #category : #running }
RBTransformationRule >> resetResult [
	builder := RBRefactoryChangeManager changeFactory compositeRefactoryChangeNamed: self name
]

{ #category : #accessing }
RBTransformationRule >> result [
	| environment |
	environment := RBParseTreeEnvironment new.
	environment matcher: self rewriteRule.
	environment label: self name.
	self changes do: [ :change |
		(change isKindOf: RBAddMethodChange)
			ifTrue: [ environment addClass: change changeClass selector: change selector ] ].
	^ environment
]

{ #category : #accessing }
RBTransformationRule >> rewriteRule [
	^ rewriteRule
]

{ #category : #accessing }
RBTransformationRule >> rewriteRule: aRule [

	rewriteRule := aRule
]
