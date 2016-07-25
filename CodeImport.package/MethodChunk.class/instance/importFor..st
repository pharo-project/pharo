importing
importFor: requestor
	self existsBehavior
		ifFalse: [ self handleMissingBehavior ].
	^ [ self targetClass
		compile: contents
		classified: categoryName
		withStamp: stamp
		notifying: (self methodCompileRequestorFor: requestor)]
			on: ReparseAfterSourceEditing
			do: [ :exception | 
				contents := exception newSource.
				self importFor: requestor  ]