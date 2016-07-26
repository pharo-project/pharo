testing
testLinesDo
	| lines reader |
	lines := #( 'foo' 'bar' 'last').
	reader := ZnFastLineReader on: (Character lf join: lines) readStream.
	self 
		assert: (Array streamContents: [ :out | 
					reader linesDo: [ :line | out nextPut: line ] ])
		equals: lines