Class {
	#name : 'ReCheckVariableNameCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'variableName',
		'parseTree'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReCheckVariableNameCondition class >> class: aRBAbstractClass variableName: aVariableName parseTree: aParseTree [ 
	^ self new class: aRBAbstractClass ; variableName: aVariableName parseTree: aParseTree; yourself
]

{ #category : 'instance creation' }
ReCheckVariableNameCondition class >> classNamed: aString inModel: aRBNamespace instanceVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		instanceVariables: aCollection;
		yourself
]

{ #category : 'checking' }
ReCheckVariableNameCondition >> check [

	(class whoDefinesInstanceVariable: variableName) ifNotNil: [ 
		violators add: ('<1p> defines an instance variable named <2s>'
			expandMacrosWith: class
         with: variableName )].
	(class whoDefinesClassVariable: variableName ) ifNotNil: [
   		violators add: ('<1p> defines a class variable named <2s>'
      		expandMacrosWith: class
         with: variableName)].
	(parseTree whoDefines: variableName)
		ifNotNil: [ 
			violators add: ('<1s> is already defined' expandMacrosWith: variableName) ].
	(parseTree allDefinedVariables includes: variableName ) ifTrue: [
		violators add: ('<1s> is already a temporary variable name'
      		expandMacrosWith: variableName )].
	^ violators isEmpty
]

{ #category : 'instance creation' }
ReCheckVariableNameCondition >> variableName: aVariableName parseTree: aParseTree [

	variableName := aVariableName.
	parseTree := aParseTree.
]

{ #category : 'displaying' }
ReCheckVariableNameCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
		aStream
			nextPutAll: ('The variable {1} is invalid in the context of the class {2}'
         		format: { violator. class name });
			nextPut: Character cr ].
]
