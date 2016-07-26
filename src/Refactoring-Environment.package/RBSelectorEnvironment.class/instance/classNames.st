accessing-classes
classNames
	^ IdentitySet new
		addAll: classSelectors keys;
		addAll: metaClassSelectors keys;
		yourself