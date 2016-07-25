event
refeshedChangeSet: changeSet
	isRefreshing
		ifTrue: [ ^ self ].
	isRefreshing := true.
	changeSet updateChangesList.
	isRefreshing := false