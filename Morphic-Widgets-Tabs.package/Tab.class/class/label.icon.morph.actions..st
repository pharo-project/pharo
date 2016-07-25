instance creation
label: label icon: icon morph: morph actions: aCollection

	^ self new
		label: label;
		icon: icon;
		morph: morph;
		addActions: aCollection;
		yourself