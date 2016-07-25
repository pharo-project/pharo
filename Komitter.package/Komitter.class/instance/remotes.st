protocol
remotes

	^ self stagingArea remotes asOrderedCollection
		add: self newSlice;
		yourself