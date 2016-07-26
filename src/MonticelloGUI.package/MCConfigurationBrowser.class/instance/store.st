actions
store
	(self checkRepositories and: [self checkDependencies]) ifFalse: [^self].
	self pickName ifNotNil: [:name |
		self configuration name: name.
		self pickRepository ifNotNil: [:repo |
			repo storeVersion: self configuration]].