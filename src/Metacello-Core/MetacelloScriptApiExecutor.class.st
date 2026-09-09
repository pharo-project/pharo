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

	| spec |
	singleRoot ifNil: [ self singleRoot: true ].

	spec := (MetacelloProjectSpec project: MetacelloProject new)
		        name: aString;
		        className: 'BaselineOf' , aString;
		        yourself.
	projectSpecBlock value: (self applyArgsToProjectSpec: spec)
]
