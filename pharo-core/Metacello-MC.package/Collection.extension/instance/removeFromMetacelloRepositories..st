*metacello-mc
removeFromMetacelloRepositories: aMetacelloRepositoriesSpec

	self do: [:each | each removeFromMetacelloRepositories: aMetacelloRepositoriesSpec ]