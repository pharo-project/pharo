initialization
initialElements
	changedClasses := Set new.
	self changes do: [ :change | change addToPackageChangeSet: self ].
	^ changedClasses collect: [:class | IceClassChangeSet class: class parent: self ]