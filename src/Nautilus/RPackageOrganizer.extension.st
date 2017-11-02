Extension { #name : #RPackageOrganizer }

{ #category : #'*Nautilus' }
RPackageOrganizer >> createPackageNamed: aString [

	| instance |
	(packages includesKey: aString asSymbol) ifTrue: [ 
			self openAlreadyExistingError.
			^ nil ].
	
	instance := self packageClass named: aString.
	self registerPackage: instance.
	^ instance
]

{ #category : #'*Nautilus' }
RPackageOrganizer >> openAlreadyExistingError [

	StandardWindow new alert: 'A package with this name already exists' title: 'Already exists'
]
