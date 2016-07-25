*metacello-mc
mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec

	self do: [:each | each mergeIntoMetacelloRepositories: aMetacelloRepositoriesSpec ]