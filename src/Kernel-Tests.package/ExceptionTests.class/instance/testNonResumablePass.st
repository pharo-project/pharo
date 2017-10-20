tests - outer
testNonResumablePass

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex pass. ex return: 5]
		] raise: Error
