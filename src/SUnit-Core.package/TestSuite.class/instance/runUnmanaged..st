running
runUnmanaged: aResult
	self setUp.
	[ self tests
		do: [ :each | 
			each run: aResult.
			self announceTest: each.
			self changed: each ] ]
		ensure: [ self tearDown ]