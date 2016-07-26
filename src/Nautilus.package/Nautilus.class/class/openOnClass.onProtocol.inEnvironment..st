opening
openOnClass: aClass onProtocol: aProtocol inEnvironment: anEnvironment

	| instance |
	instance := self new.
	instance 
		browsedEnvironment: anEnvironment;
		showInstance: aClass isMeta not;
		selectedCategory: aProtocol;
		package: aClass package class: aClass;
		open.
	instance ui 
		showHierarchy: false.
	instance ui 
		selectedCategory: aProtocol;
		categorySelectionChanged;
		changed: #currentHistoryIndex.
	^ instance