"
I'm a condition that checks if given variable name is shadowing an instance variable/class variable of given class.
"
Class {
	#name : 'ReCheckVariableNameShadowingCondition',
	#superclass : 'ReClassCondition',
	#instVars : [
		'variableName',
		'parseTree'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'instance creation' }
ReCheckVariableNameShadowingCondition class >> class: aRBAbstractClass variableName: aVariableName parseTree: aParseTree [ 
	^ self new class: aRBAbstractClass ; variableName: aVariableName parseTree: aParseTree; yourself
]

{ #category : 'instance creation' }
ReCheckVariableNameShadowingCondition class >> classNamed: aString inModel: aRBNamespace instanceVariables: aCollection [ 
	
	^ (self classNamed: aString inModel: aRBNamespace)
		instanceVariables: aCollection;
		yourself
]

{ #category : 'checking' }
ReCheckVariableNameShadowingCondition >> check [
    (class whoDefinesInstanceVariable: variableName) ifNotNil: [
        violators addIfNotPresent: variableName ].

    (class whoDefinesClassVariable: variableName) ifNotNil: [
        violators addIfNotPresent: variableName ].

    ^ violators isEmpty

]

{ #category : 'instance creation' }
ReCheckVariableNameShadowingCondition >> variableName: aVariableName parseTree: aParseTree [

	variableName := aVariableName.
	parseTree := aParseTree.
]

{ #category : 'displaying' }
ReCheckVariableNameShadowingCondition >> violationMessageOn: aStream [

	^ violators do: [ :violator |
			  aStream
				  nextPutAll: ('The variable {1} is already defined as a class/instance variable in the given context' format: { violator });
				  nextPut: Character cr ]
]
