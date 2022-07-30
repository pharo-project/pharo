Class {
	#name : #OCNewCompilerWithChangesFunctionalTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #tests }
OCNewCompilerWithChangesFunctionalTest >> testPragma [
	| aCompiledMethod bytecode |
	aCompiledMethod := OpalCompiler new
		class: OCMockCompilationClass;
		compile: 'methodDoublePragma
											<hello: 5>
											<hello: 2>'.

	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	self assert: bytecode first equals: 'Quick return self'
]
