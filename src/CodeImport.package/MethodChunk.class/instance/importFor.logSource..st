importing
importFor: requestor logSource: logSource
	self existsBehavior
		ifFalse: [ self handleMissingBehavior ].
	^ [ self targetClass
		compile: contents
		classified: categoryName
		withStamp: stamp
		notifying: (self methodCompileRequestorFor: requestor)
		logSource: logSource ]
		on: ReparseAfterSourceEditing
		do: [ :exception | 
			contents := exception newSource.
			self importFor: requestor logSource: logSource ]