tests
testConnectedToTranscript

	| model transcript |

	model := TestableTranscriptRubricOutput new.
	model createTextMorph.
		
	transcript := TranscriptStream new.
	
	transcript addOutput: model.

	transcript show: 'OllehDlorw'.
	
	self 
	assert: model stream contents = 'OllehDlorw';
	assert: model textMorph getText = '';
	assert: model hasBeenCleared == false.
	
	model checkForUpdate.
	
	self 
	assert: model stream contents = '';
	assert: model textMorph getText = 'OllehDlorw';
	assert: model hasBeenCleared == false.
	
	transcript clear.
	
	self
	assert: model stream contents = '';
	assert: model textMorph getText = 'OllehDlorw';
	assert: model hasBeenCleared == true.
	
	model checkForUpdate.
	
	self
	assert: model stream contents = '';
	assert: model textMorph getText = '';
	assert: model hasBeenCleared == false;
	assert: model noModelStuff.
	
	
