testing
testLoadAndUnload
	|definition|
	definition := self mockMethod: #one class: 'MCMockClassA' source: 'one ^2' meta: false.
	self assert: self mockInstanceA one = 1.
	definition load.
	self assert: self mockInstanceA one = 2.
	definition unload.
	self deny: (self mockInstanceA respondsTo: #one)