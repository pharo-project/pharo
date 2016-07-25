accessing-classes
classNames
	^ IdentitySet new
		addAll: classes;
		addAll: metaClasses;
		yourself