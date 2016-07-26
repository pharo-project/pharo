transforming
transform
	renameAccessors ifTrue: [ 
		self removeOldAccessors 
	].
	
	class renameInstanceVariable: variableName to: newName around: [ self renameReferences ].
	
	renameAccessors ifTrue: [ 
		self addNewAccessors.
		self renameAccessorsReferences
	]