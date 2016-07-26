development support
packagesNeedSavingVisited: visitedProjects into: aCollection

	self packages map valuesDo: [:pkg | pkg packagesNeedSavingVisited: visitedProjects using: self repositories map values into: aCollection ].