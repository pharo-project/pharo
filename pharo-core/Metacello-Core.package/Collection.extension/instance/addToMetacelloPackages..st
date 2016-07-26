*Metacello-Core
addToMetacelloPackages: aMetacelloPackagesSpec

	self do: [:each | each addToMetacelloPackages: aMetacelloPackagesSpec ]