private
announceRepositoryAdded: aRepository
	SystemAnnouncer uniqueInstance
		announce:
			(MCRepositoryAdded new
				repository: aRepository;
				yourself)