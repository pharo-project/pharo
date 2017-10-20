tests - formatting
testExpandMacros

	self
		assert: '<t><n><r>' expandMacros
		equals:
			(String
				streamContents: [ :stream | 
					stream
						nextPut: Character tab;
						nextPutAll: OSPlatform current lineEnding;
						nextPut: Character cr ])