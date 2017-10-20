tests - outer
testNonResumableOuter

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex outer. ex return: 5]
		] raise: Error
