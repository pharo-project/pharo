private
objectClassTab
	| tab |
	tab := self newTab.
	tab
		label: 'Object class';
		icon: (tab iconNamed: #nautilusIcon);
		model: (MessageBrowser new messages: Object methods).
	^ tab