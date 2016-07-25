*Metacello-Core
mergeIntoMetacelloPackages: aMetacelloPackagesSpec

	self do: [:each | each mergeIntoMetacelloPackages: aMetacelloPackagesSpec ]