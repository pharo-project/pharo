pages
running
	"This method was automatically generated. Edit it using:"
	"SUnitHelp edit: #running"
	^HelpTopic
		title: 'Running tests'
		contents: 
'SUnit allows writing tests on Smalltalk source code and checking results. This is usually done in a tool called TestRunner which you can either open from the Pharo world menu or by evaluating:

   TestRunner open

in a Smalltalk workspace.

By writing tests and checking them with the test runner you can ensure the sustainability of your applications.

!' readStream nextChunkText