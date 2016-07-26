cleanup
removeFrom: aNode

	(self allWatchpoints at: aNode ifAbsent: [ ^self ]) uninstall