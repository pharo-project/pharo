action
save: aValue
	| oldElement |
	oldElement := self host array at: self index.
	self host 
		remove: oldElement;
		add: aValue.