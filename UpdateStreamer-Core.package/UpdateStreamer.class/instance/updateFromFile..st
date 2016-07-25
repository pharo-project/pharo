public
updateFromFile: aFileReference
	self updateWith: [ 
		self elementaryReadServerUpdates: (self newUpdatesFromFile: aFileReference) ]