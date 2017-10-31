tests
testUpdateStrategy

	| transcript output |
	
	transcript := TranscriptStream new.
	output := TestableTranscriptOutput new.
	
	transcript addOutput: output.
	
	transcript updateUsing: TranscriptUpdateStrategy new while: [  ].
	
	self 
	assert: output data = '';
	assert: output apiCount = { 1 . 0 . 0 . 0 . 0 }.
	
	transcript updateUsing: TranscriptUpdateStrategy new while: [ 
		transcript show: 'Updated'.
			
		self 
		assert: output data = 'Updated'; 		
		assert: output apiCount = { 1 . 1 . 0 . 0 . 1 }.
		
		transcript updateUsing: nil while: [
			transcript show: '_u2-'.
			
			self 
			assert: output data = 'Updated_u2-'; 		
			assert: output apiCount = { 1 . 2 . 0 . 0 . 1 }.
			
		].
		
		transcript clear; close.		
		
		self 
		assert: output data = 'Updated_u2-'; 		
		assert: output apiCount = { 1 . 3 . 1 . 1 . 3 }.
		
	].