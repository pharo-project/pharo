private
selectedPackageRepositories

	^ packages selectedItem
		ifNil: [ #() ]
		ifNotNil: [ :item | item content repositoryGroup repositories ].