"
I am a refactoring that removes all unused temporary variables from a method.
"
Class {
	#name : 'ReRemoveUnusedTemporaryVariableRefactoring',
	#superclass : 'ReRefactoring',
	#instVars : [
		'selector',
		'class',
		'parseTree'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'instance creation' }
ReRemoveUnusedTemporaryVariableRefactoring class >> model: aRBNamespace inMethod: aSelector inClass: className [ 
	
	^ self new
		model: aRBNamespace;
		inMethod: aSelector
		inClass: className
]

{ #category : 'initialization' }
ReRemoveUnusedTemporaryVariableRefactoring >> inMethod: aSelector inClass: className [

	selector := aSelector asSymbol.
	class := self model classNamed: className
]

{ #category : 'transforming' }
ReRemoveUnusedTemporaryVariableRefactoring >> prepareForExecution [ 

	parseTree := class parseTreeForSelector: selector
]

{ #category : 'transforming' }
ReRemoveUnusedTemporaryVariableRefactoring >> privateTransform [

	| unusedTemps |
	unusedTemps := parseTree allTemporaryVariables reject: [ :temp | parseTree references: temp ].
	unusedTemps do: [ :temp |
		(RBRemoveTemporaryVariableTransformation
			model: self model
			variable: temp
			inMethod: selector
			inClass: class) generateChanges ]
]
