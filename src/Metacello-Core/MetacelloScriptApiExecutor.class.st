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
MetacelloScriptApiExecutor >> executeString: aString do: projectSpecBlock [

  singleRoot ifNil: [ self singleRoot: true ].
  (projectSpecGenerator projectSpecCreationBlock value: aString)
    do: [ :projectSpec | 
      projectSpec
        ifNotNil: [ projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ] ]
]
