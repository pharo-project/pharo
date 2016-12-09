method-compiling
createNewMethod: aMethodDefinition

"	| ownerClass |
""	ownerClass := builder behaviorFrom: aMethodDefinition methodClass theNonMetaClass."
"	aMethodDefinition methodClass isMeta ifTrue: [ ownerClass := ownerClass classSide ].
"
	| method timeToCompile |
	timeToCompile := [ method := builder objectSpace crossCompiler
		compile: aMethodDefinition sourceCode
		forClass: aMethodDefinition methodClass
		inEnvironment: builder] timeToRun.
	builder statistics addCompilationTime: timeToCompile.
	^ method