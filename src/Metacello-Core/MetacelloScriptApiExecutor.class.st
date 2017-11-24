"
MetacelloScriptExecutor decodes script args and launches MetacelloScriptEngine to run for each projectSpec encounters...i,e., primarily here to handl array and block args to configuration:, baseline: or project:.
"
Class {
	#name : #MetacelloScriptApiExecutor,
	#superclass : #MetacelloScriptExecutor,
	#category : #'Metacello-Core-Scripts'
}

{ #category : #'execution callback' }
MetacelloScriptApiExecutor >> executeBlock: selectBlock do: projectSpecBlock [
    (self projectSpecsFromRepositoryArg select: selectBlock)
        do: [ :projectSpec | projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ]
]

{ #category : #'execution callback' }
MetacelloScriptApiExecutor >> executeCollection: aCollection do: projectSpecBlock [
    self projectSpecsFromRepositoryArg
        do: [ :projectSpec | 
            (aCollection includes: (MetacelloScriptEngine baseNameOf: projectSpec className))
                ifTrue: [ projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ] ]
]

{ #category : #'execution callback' }
MetacelloScriptApiExecutor >> executeString: aString do: projectSpecBlock [
  singleRoot ifNil: [ self singleRoot: true ].
  (projectSpecGenerator projectSpecCreationBlock value: aString)
    do: [ :projectSpec | 
      projectSpec
        ifNotNil: [ projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ] ]
]

{ #category : #'execution callback' }
MetacelloScriptApiExecutor >> projectSpecsFromRepositoryArg [
    | spec repo projectSpecs |
    repositoryArg ifNil: [ ^ #() ].
    spec := (projectSpecGenerator projectSpecCreationBlock value: 'xxx') first.
    repo := spec project createRepository: (spec project repositorySpec description: repositoryArg).
    projectSpecs := OrderedCollection new.
    ((Gofer new disablePackageCache
        repository: repo;
        allResolved) collect: [ :resolvedReference | resolvedReference packageName ]) asSet
        do: [ :packageName | 
            (projectSpecGenerator projectSpecCreationBlock value: (MetacelloScriptEngine baseNameOf: packageName))
                do: [ :projectSpec | 
                    projectSpec className = packageName
                        ifTrue: [ projectSpecs add: (self applyArgsToProjectSpec: projectSpec copy) ] ] ].
    ^ projectSpecs
]
