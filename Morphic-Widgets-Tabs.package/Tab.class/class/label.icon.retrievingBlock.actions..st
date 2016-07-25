instance creation
label: label icon: icon retrievingBlock: morph actions: aCollection

	^ self new
		label: label;
		icon: icon;
		retrievingBlock: morph;
		addActions: aCollection;
		yourself