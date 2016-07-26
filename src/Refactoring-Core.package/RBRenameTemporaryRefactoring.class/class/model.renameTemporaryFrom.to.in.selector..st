instance creation
model: aRBSmalltalk renameTemporaryFrom: anInterval to: newName in: aClass selector: aSelector 
	^(self new)
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			interval: anInterval
			newName: newName;
		yourself