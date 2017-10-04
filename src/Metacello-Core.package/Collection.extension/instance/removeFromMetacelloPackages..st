*metacello-core
removeFromMetacelloPackages: aMetacelloPackagesSpec

	self do: [:each | each removeFromMetacelloPackages: aMetacelloPackagesSpec ]