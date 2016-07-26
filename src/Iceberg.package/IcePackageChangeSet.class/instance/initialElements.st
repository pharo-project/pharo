initialization
initialElements
	^ self changedClasses collect: [:class | IceClassChangeSet class: class parent: self ]
