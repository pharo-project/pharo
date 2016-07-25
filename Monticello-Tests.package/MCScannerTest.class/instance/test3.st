tests
test3
	self assert: (MCScanner scan: '(a #b c)' readStream) = #(a #b c)