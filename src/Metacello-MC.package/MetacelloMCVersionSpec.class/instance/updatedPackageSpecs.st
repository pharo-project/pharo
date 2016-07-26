development support
updatedPackageSpecs

	| updatedSpecs mcLoader |
	updatedSpecs := Dictionary new.
	mcLoader := self loader.
	self packages map valuesDo: [:pkg | pkg updatePackageSpec: updatedSpecs using: mcLoader].
	^updatedSpecs