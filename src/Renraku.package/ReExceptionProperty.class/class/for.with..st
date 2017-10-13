instance creation
for: anEntity with: anException
	
	^ self new
		initializeSourceAnchor: (
			ReSourceAnchor entity: anEntity);
		stack: anException signalerContext copyStack;
		message: anException description;
		yourself
		