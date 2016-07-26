*Metacello-Core
removeFromMetacelloPackages: aMetacelloPackagesSpec

	self do: [:each | each removeFromMetacelloPackages: aMetacelloPackagesSpec ]