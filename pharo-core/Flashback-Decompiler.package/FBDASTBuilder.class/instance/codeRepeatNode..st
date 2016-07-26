building
codeRepeatNode: body
	^ (self
		codeMessage: #repeat
		receiver: (self codeBlock: body)
		arguments: #() ) 
			propertyAt: #reconstructed put: true;
			yourself