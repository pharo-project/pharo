instance creation
for: anEntity by: aRule

	^ self basicNew
		initializeRule: aRule
		target: anEntity;
		yourself