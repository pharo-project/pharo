A CompilerNotifyingTest is a TestCase for checking that Compiler/Parser notifications are inserted at the right place in a TextEditor.

Instance Variables
	expectedErrorPositions:		<Array of: Integer>
	expectedErrors:		<Array of: String>
	failure:		<Object>
	morph:		<TextMorph>
	text:		<String>

errorPositions
	- the position where error text should be inserted for each chunk of text evaluated

errors
	- the error text that should be inserted on evaluation of each chunk of text evaluated

failure
	- an object returned in case of evaluation error and whose identity can be uniquely recognized as a failure
	
morph
	- the Morph holding the text
	
text
	- the string containing all the chunks to be evaluated (separated by %)
	  and the expected error messages (`enclosed in back quotes`)
	  this text will be stripped of the error messages before being evaluated.

