*metacello-core
addToMetacelloPackages: aMetacelloPackagesSpec

	self do: [:each | each addToMetacelloPackages: aMetacelloPackagesSpec ]