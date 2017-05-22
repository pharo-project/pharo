instance creation
for: aResponse
	^ (self errorClassFor: aResponse) new 
		response: aResponse;
		yourself	