refactory-changes
removeMethod: aSelector from: aClass 
	^ self addChange: (RBRemoveMethodChange remove: aSelector from: aClass)