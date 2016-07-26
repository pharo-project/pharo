running
tearDown
	(self hasPackage: 'GoferFoo')
		ifTrue: [ Gofer new package: 'GoferFoo'; unload ].
	(self hasPackage: 'GoferBar')
		ifTrue: [ Gofer new package: 'GoferBar'; unload ].
	(self hasPackage: 'ConfigurationOfGoferFoo')
		ifTrue: [ Gofer new package: 'ConfigurationOfGoferFoo'; unload ]