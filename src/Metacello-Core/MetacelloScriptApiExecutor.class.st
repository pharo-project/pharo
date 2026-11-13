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
MetacelloScriptApiExecutor >> rootFor: aProjectSpec [
	"A script of Metacello new works on the spec it names, registered or not"

	^ aProjectSpec
]
