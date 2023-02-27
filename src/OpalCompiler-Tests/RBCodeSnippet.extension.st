Extension { #name : #RBCodeSnippet }

{ #category : #'*OpalCompiler-Tests' }
RBCodeSnippet >> compile [

	^ OpalCompiler new
		  options: #( #optionParseErrors );
		  noPattern: true;
		  compile: self source
]
