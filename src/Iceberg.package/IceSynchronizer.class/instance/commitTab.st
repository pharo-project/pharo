private
commitTab
	^ self newTab
		label: 'Commit current changes';
		icon: (Smalltalk ui icons iconNamed: #nautilusIcon);
		model: self commitBrowser;
		yourself