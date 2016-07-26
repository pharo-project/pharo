plugin interface
for: aSelector
	^ self subclasses detect: [ :class | class key = aSelector ]