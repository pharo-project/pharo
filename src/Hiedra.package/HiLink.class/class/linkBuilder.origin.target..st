instance creation
linkBuilder: aLinkBuilder origin: aNode target: anotherNode

	^ self basicNew
		initializeLinkBuilder: aLinkBuilder origin: aNode target: anotherNode;
		yourself