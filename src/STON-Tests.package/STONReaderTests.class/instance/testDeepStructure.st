tests
testDeepStructure
	| holder deepest structure writer ston reader result |
	"Create a deep nested structure so that the deepest element is a reference back to a top level holder."
	holder := Array with: 42.
	deepest := Array with: holder.
	structure := deepest.
	64 * 1024 timesRepeat: [ structure := Array with: structure ].
	structure := Array with: holder with: structure.
	writer := STON writer optimizeForLargeStructures.
	ston := String streamContents: [ :out | (writer on: out) nextPut: structure ].
	"After reading, the second pass will have to go down the structure to resolve the reference."
	reader := STON reader optimizeForLargeStructures.
	result := (reader on: ston readStream) next.
	self assert: result equals: structure