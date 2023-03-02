Extension { #name : #RBCodeSnippet }

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippet >> compile [

	^ OpalCompiler new
		  options: #( #optionParseErrors );
		  noPattern: isMethod not;
		  compile: self source
]
