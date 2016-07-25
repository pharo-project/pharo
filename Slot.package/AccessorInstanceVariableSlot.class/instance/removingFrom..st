class building
removingFrom: aClass
	
	aClass removeSelector: self name.
	aClass removeSelector: self name asMutator.
	