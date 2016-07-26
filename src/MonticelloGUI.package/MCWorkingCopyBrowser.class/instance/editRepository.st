actions
editRepository
	| newRepo |
	newRepo := self repository openAndEditTemplateCopy.
	newRepo ifNotNil: [ 
		(newRepo isKindOf: MCRepository)
			ifFalse: [ 
				self inform: 'Got a ', newRepo class name, ' instead of a Monticello Repository'].
		self flag: 'Is this ugly? yes!'.
		self repository becomeForward: newRepo ].
	self repositoryListChanged