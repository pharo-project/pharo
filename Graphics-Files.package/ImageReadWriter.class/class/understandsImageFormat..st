image reading/writing
understandsImageFormat: aStream 
	^[(self new on: aStream) understandsImageFormat] on: Error do:[:ex| ex return: false]