private
browserTab
	| tab |
	tab := self newTab.
	tab
		label: 'Browser';
		icon: (tab iconNamed: #nautilusIcon);
		model:
			(ClassMethodBrowser new
				classes: Smalltalk allClasses;
				yourself).
	^ tab