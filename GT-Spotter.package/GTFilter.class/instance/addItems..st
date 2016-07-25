private-model
addItems: aCollection
	self context streamed addAll: aCollection.
	self context addItems: aCollection