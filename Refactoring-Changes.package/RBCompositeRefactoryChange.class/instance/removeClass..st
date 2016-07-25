refactory-changes
removeClass: aClass 
	^ self addChange: (RBRemoveClassChange removeClassName: aClass name)