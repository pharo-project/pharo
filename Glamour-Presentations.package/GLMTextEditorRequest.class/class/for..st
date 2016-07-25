instance creation
for: aSelector

	^ self new action: [ :editor | editor perform: aSelector ]