instance creation
newAcceptButtonFor: aPlayPage
	^ self newActionButton
		hintTitle: 'Accept';
		hintText: 'load saved page into playground';
		icon: (Smalltalk ui icons iconNamed: #glamorousAccept);
		action: [ :b | 
			self remove.
			self announce: #pageAccepted event: aPlayPage ]