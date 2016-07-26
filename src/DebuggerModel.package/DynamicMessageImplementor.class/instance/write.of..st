evaluating-private
write: aKeyword of: anIndex

	stream nextPutAll: aKeyword.
	self writeArgumentNameIfNecessaryOf: aKeyword at: anIndex