"
This smell arises when a class variable is not both read and written. If a class variable is only read, the reads can be replaced by nil, since it could not have been assigned a value. If the variable is only written, then it does not need to store the result since it is never used.

If you really intend to have an unused variable (for example, for testing purpose), you can add a #ignoreUnusedClassVariables: pragma to your initialize method with an array of unused variables. Example: `<ignoreUnusedClassVariables: #( ClassVar1 ClassVar2 )>`.
"
Class {
	#name : 'ReClassVariableNeitherReadNorWrittenRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Clean Code',
	#package : 'General-Rules',
	#tag : 'Clean Code'
}

{ #category : 'testing-interest' }
ReClassVariableNeitherReadNorWrittenRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReClassVariableNeitherReadNorWrittenRule class >> group [
	^ 'Clean Code'
]

{ #category : 'accessing' }
ReClassVariableNeitherReadNorWrittenRule class >> ruleName [
	^ 'Class variable not read or not written'
]

{ #category : 'accessing' }
ReClassVariableNeitherReadNorWrittenRule class >> severity [
	^ #information
]

{ #category : 'running' }
ReClassVariableNeitherReadNorWrittenRule >> check: aClass forCritiquesDo: aCriticBlock [

	(self unreferencedVariablesOf: aClass)
		reject: [ :variable |
			variable definingClass pragmas anySatisfy: [ :pragma |
				pragma selector = #ignoreUnusedClassVariables: and: [ (pragma argumentAt: 1) includes: variable name ] ] ]
		thenDo: [ :variable | aCriticBlock cull: (self critiqueFor: aClass about: variable name) ]
]

{ #category : 'running' }
ReClassVariableNeitherReadNorWrittenRule >> unreferencedVariablesOf: aClass [

	^ aClass classVariables reject: [ :variable | variable isReferenced ]
]
