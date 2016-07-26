protocol
remotesFor: aCollection

	^ (self stagingArea remotesFor: aCollection) asOrderedCollection
		add: self newSlice;
		yourself