private
repositoryFor: projectName
	| repositories repository |
	repositories := self repositories
		select: [ :repo | (repo description beginsWith: 'http:') and: [ repo description endsWith: projectName ] ].
	repository := nil.
	(repositories anySatisfy: [ :http | http description = ('http://www.squeaksource.com/' , projectName) ])
		ifFalse: [ 
			(self uiManager confirm: 'Would you like to use http://www.squeaksource.com/' , projectName , ', for your project?')
				ifTrue: [ ^ MCHttpRepository location: 'http://www.squeaksource.com/' , projectName user: '' password: '' ] ].
	repository isNil
		ifTrue: [ 
			repository := repositories isEmpty
				ifTrue: [ self chooseRepositoryFromList: self repositories ]
				ifFalse: [ self chooseRepositoryFromList: repositories , self repositories ] ].
	^ repository