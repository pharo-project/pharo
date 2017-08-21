tests
testPrintPathOnDelimiter

	| pathString pathSrc path |

	"Test a Relative path"
	"Use an unusal delimiter to check that the default isn't hardcoded anywhere"
	pathSrc := 'one|two|three'.
	path := Path from: pathSrc delimiter: $|.
	self assert: path isRelative.
	pathString := String streamContents: [ :stream | path printPathOn: stream delimiter: $| ].
	self assert: pathSrc equals: pathString.

	"Test an Absolute path"
	"Use an unusal delimiter to check that the default isn't hardcoded anywhere"
	pathSrc := '|one|two|three'.
	path := Path from: pathSrc delimiter: $|.
	self assert: path isAbsolute.
	pathString := String streamContents: [ :stream | path printPathOn: stream delimiter: $| ].
	self assert: pathSrc equals: pathString