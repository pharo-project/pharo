running
setUpThreeEntriesWithAbsentPrior

	threeEntriesWithAbsentPrior := 
		EpLogBuilder new 
			logAbsentInitial;
			logChildren: 3 triggerReference: OmNullReference uniqueInstance;
			log.