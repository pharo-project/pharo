accessing-classes
classNames
	^ self categories 
		inject: IdentitySet new
		into: [ :answer :each | 
			answer
				addAll: (self classNamesFor: each);
				yourself ]