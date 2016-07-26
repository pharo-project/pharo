cleanup
deleteHistoryFrom: aNode

	(self allWatchpoints at: aNode ifAbsent:[^self]) deleteHistory