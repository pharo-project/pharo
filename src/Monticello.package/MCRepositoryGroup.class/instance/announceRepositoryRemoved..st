private
announceRepositoryRemoved: aRepository
	SystemAnnouncer uniqueInstance
		announce:
			(MCRepositoryRemoved new
				repository: aRepository;
				yourself)