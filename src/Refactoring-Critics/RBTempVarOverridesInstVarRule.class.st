"
Finds methods whose temporary variables override an instance variable. This causes problems if you want to use the instance variable inside the method.
"
Class {
	#name : #RBTempVarOverridesInstVarRule,
	#superclass : #RBBlockLintRule,
	#instVars : [
		'matcher',
		'varName',
		'vars'
	],
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBTempVarOverridesInstVarRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBTempVarOverridesInstVarRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'TempVarOverridesInstVarRule'
]

{ #category : #running }
RBTempVarOverridesInstVarRule >> basicCheck: aMethod [ 
	vars := aMethod methodClass instVarNames.
	^ (matcher 
		executeTree: aMethod ast
		initialAnswer: false)
]

{ #category : #running }
RBTempVarOverridesInstVarRule >> checkMethod: aMethod [ 
	(self basicCheck: aMethod) ifTrue: 
		[ result 
			addMethod: aMethod;
		 	addSearchString: varName ]
]

{ #category : #accessing }
RBTempVarOverridesInstVarRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBTempVarOverridesInstVarRule >> initialize [
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher
		matchesArgument: '`var'
		do: [ :aNode :answer | 
			answer or: [
				varName := aNode name.
				vars includes: varName ]]
]

{ #category : #accessing }
RBTempVarOverridesInstVarRule >> name [
	^ 'Instance variable overridden by temporary variable'
]

{ #category : #copying }
RBTempVarOverridesInstVarRule >> postCopy [
	super postCopy.
	self initialize.
]
