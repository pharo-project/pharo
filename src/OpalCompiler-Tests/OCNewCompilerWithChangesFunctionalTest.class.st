Class {
	#name : 'OCNewCompilerWithChangesFunctionalTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Misc',
	#package : 'OpalCompiler-Tests',
	#tag : 'Misc'
}

{ #category : 'tests' }
OCNewCompilerWithChangesFunctionalTest >> testPragma [
	| aCompiledMethod bytecode |
	aCompiledMethod := OpalCompiler new
		source:
			'methodDoublePragma
											<hello: 5>
											<hello: 2>';
		class: OCMockCompilationClass;
		compile.

	bytecode := aCompiledMethod symbolic asString substrings: String cr.
	self assert: bytecode first equals: 'Quick return self'
]
