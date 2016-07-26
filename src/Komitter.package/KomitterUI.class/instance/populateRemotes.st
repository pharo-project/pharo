initialization
populateRemotes
	
	remotes 
		items: self model remotes asOrderedCollection;
		displayBlock: [ :each | each label ];
		iconHolder: [ :each | each icon ]