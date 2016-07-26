accessing
menuProvider: anObject selector: aSelector
	self textArea
		getMenuPolicy:
			(RubPluggableGetMenuPolicy new
				getMenuSelector: aSelector;
				menuProvider: anObject;
				yourself)