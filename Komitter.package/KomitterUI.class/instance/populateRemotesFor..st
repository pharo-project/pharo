initialization
populateRemotesFor: aCollection
	
	remotes 
		items: (self model remotesFor: aCollection) asOrderedCollection;
		displayBlock: [ :each | each label ];
		iconHolder: [ :each | each icon ]