private
collectPackages: addedPackages andProjects: addedConfigurations pattern: patternString repository: repository
	| packagePattern package |
	packagePattern := self  uiManager
		request: 'Please enter a pattern to filter package names that will be candidates to add to configuration'
		initialAnswer: patternString.
	packagePattern ifNil: [ ^ self ].
	[ 
	| packageNames workingCopies |
	packageNames := OrderedCollection new.
	workingCopies := OrderedCollection new.
	self workingCopies
		do: [ :pkg | 
			((packagePattern match: pkg package name) and: [ (addedPackages includes: pkg package name) not ])
				ifTrue: [ 
					packageNames add: pkg package name.
					workingCopies add: pkg ] ].
	package := self uiManager
		chooseFrom: packageNames
		values: workingCopies
		title: 'Add dependent package (cancel to stop)'.
	package
		ifNotNil: [ 
			(package package name beginsWith: 'ConfigurationOf')
				ifTrue: [ addedConfigurations add: package package name ]
				ifFalse: [ addedPackages add: package package name ].
			package ancestry ancestors isEmpty
				ifTrue: [ 
					(self uiManager
						confirm:
							'The package: ' , package package name printString , ' has not been committed yet. Would you like to commit it into the '
								, repository description printString, ' repository?')
						ifTrue: [ 
							package repositoryGroup repositories size <= 1
								ifTrue: [ package repositoryGroup addRepository: repository ].
							package currentVersionInfo ] ] ].
	package notNil ] whileTrue.
	(addedPackages isEmpty and: [ addedConfigurations isEmpty ])
		ifTrue: [ self uiManager inform: 'No package or configuration added. No baseline has been created' ]