protocol
pwd
	| result |
	self sendCommand: 'PWD'.
	self lookForCode: 257.
	result := self lastResponse.
	^result copyFrom: (result indexOf: $")+1 to: (result lastIndexOf: $")-1