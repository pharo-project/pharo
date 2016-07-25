running
tearDownPackages: aGoferInstance

	(self hasPackage: 'GoferFoo')
		ifTrue: [ aGoferInstance package: 'GoferFoo' ].
	(self hasPackage: 'GoferBar')
		ifTrue: [ aGoferInstance package: 'GoferBar' ].
	(self hasPackage: 'GoferFaux')
		ifTrue: [ aGoferInstance package: 'GoferFaux' ].
	(self hasPackage: 'GoferBeau')
		ifTrue: [ aGoferInstance package: 'GoferBeau' ].
	(self hasPackage: 'MetacelloTestConfigurationOfFoo')
		ifTrue: [ aGoferInstance package: 'MetacelloTestConfigurationOfFoo' ].
