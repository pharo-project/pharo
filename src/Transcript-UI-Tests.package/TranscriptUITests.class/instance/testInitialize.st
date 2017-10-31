tests
testInitialize

	| model |
	
	model := TestableTranscriptRubricOutput new.
	
	self 
	assert: model hasBeenCleared == false;
	assert: model textMorph == nil;
	assert: model connections class == WeakSet;
	assert: model connections asArray isEmpty;
	assert: model stream class == WriteStream;
	assert: model stream collectionSpecies == ByteString;
	assert: model stream contents = '';
	assert: model noModelStuff.