*metacello-core
mergeIntoMetacelloPackages: aMetacelloPackagesSpec

	self do: [:each | each mergeIntoMetacelloPackages: aMetacelloPackagesSpec ]