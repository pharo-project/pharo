"
MetacelloScriptExecutor decodes script args and launches MetacelloScriptEngine to run for each projectSpec encounters...i,e., primarily here to handl array and block args to configuration:, baseline: or project:.
"
Class {
	#name : 'MetacelloScriptApiExecutor',
	#superclass : 'MetacelloScriptExecutor',
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'execution callback' }
MetacelloScriptApiExecutor >> execute [

	MetacelloPlatform current withMetacelloLoadSessionDo: [ ^ super execute ]
]

{ #category : 'execution callback' }
MetacelloScriptApiExecutor >> executeString: aString do: projectSpecBlock [

	| projectSpec spec |
	singleRoot ifNil: [ self singleRoot: true ].

	projectSpec := MetacelloProject new baselineOfProjectSpec
		               name: aString;
		               className: 'BaselineOf' , aString;
		               yourself.
	spec := MetacelloProject new projectReferenceSpec
		        name: aString;
		        projectReference: projectSpec;
		        yourself.

	projectSpecBlock value: (self applyArgsToProjectSpec: spec copy)
]
