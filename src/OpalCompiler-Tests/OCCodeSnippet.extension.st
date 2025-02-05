Extension { #name : 'OCCodeSnippet' }

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
