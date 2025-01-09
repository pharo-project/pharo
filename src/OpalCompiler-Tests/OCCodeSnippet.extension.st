Extension { #name : 'OCCodeSnippet' }

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippet >> compile [

	^ [ OpalCompiler new
		  permitFaulty: true;
		  isScripting: isScripting;
		  compile: self source ]
	on: OCCodeError do: [ :e |
		"Compilation should success, because its the *faulty* mode".
		"If this is expected, then just return nil"
		self ifSkip: #compile then: [^ nil ].
		"Otherwise, pass the error"
		e pass ]
]

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippet >> compileOnError: aBlock [

	^ [ OpalCompiler new
		  isScripting: isScripting;
		  compile: self source ] on: OCCodeError do: [ :e | aBlock cull: e ]
]

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippet >> doSemanticAnalysis [

	"Do the semantic analysis as with the compiler and return the AST of the whole method (possibly a DoIt method)"

	"The responsability of scope and context is far from clear.
	* using `ASTMethodNode>>#doSemanticAnalysis` we cannot configure it.
	* using `OCASTSemanticAnalyzer` just fails on uninitialised internal thing.
	So just ask the compiler."

	^ OpalCompiler new
		  isScripting: isScripting;
		  parse: self source "Note: `parse:` also does the semantic analysis and return the AST"
]

{ #category : '*OpalCompiler-Tests' }
OCCodeSnippet >> doSemanticAnalysisOnError: aBlock [

	^ [ OpalCompiler new
		  isScripting: isScripting;
		  permitFaulty: false;
		  parse: self source ] on: OCCodeError do: [ :e | aBlock value: e ]
]
