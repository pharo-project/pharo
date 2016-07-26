private
remoteRepository
	| repositoryWithoutSlash |
	
	repositoryWithoutSlash  := self repository last = $/
		ifTrue: [ self repository allButLast ]
		ifFalse: [ self repository ].
	
	^ MCRepositoryGroup default repositories
		detect: [:each | 
			(each description = repositoryWithoutSlash) 
			or: [ each description = (repositoryWithoutSlash, '/') ] ]
		ifNone: [
			MCHttpRepository
				location: self repository
				user: ''
				password: '' ]