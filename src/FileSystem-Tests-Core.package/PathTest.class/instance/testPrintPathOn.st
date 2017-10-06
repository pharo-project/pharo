tests
testPrintPathOn

	| pathString pathSrc path |

	"Test a Relative path"
	pathSrc := 'one/two/three'.
	path := Path from: pathSrc.
	self assert: path isRelative.
	pathString := String streamContents: [ :stream | path printPathOn: stream ].
	self assert: pathSrc equals: pathString.

	"Test an Absolute path"
	pathSrc := '/one/two/three'.
	path := Path from: pathSrc.
	self assert: path isAbsolute.
	pathString := String streamContents: [ :stream | path printPathOn: stream ].
	self assert: pathSrc equals: pathString