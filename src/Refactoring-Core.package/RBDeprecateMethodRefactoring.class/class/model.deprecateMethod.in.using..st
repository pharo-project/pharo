intance creation
model: aRBSmalltalk deprecateMethod: aSelector in: aClass using: newSelector
	^(self new)
		model: aRBSmalltalk;
		deprecateMethod: aSelector
			in: aClass
			using: newSelector;
		yourself