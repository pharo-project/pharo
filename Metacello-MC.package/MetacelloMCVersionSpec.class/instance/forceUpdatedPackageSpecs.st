development support
forceUpdatedPackageSpecs

	| updatedSpecs mcLoader |
	updatedSpecs := Dictionary new.
	mcLoader := self loader.
	self packages map valuesDo: [:pkg | pkg forceUpdatePackageSpec: updatedSpecs using: mcLoader].
	^updatedSpecs