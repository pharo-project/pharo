cleanup
removeFrom: aNode

	(self allCounters at: aNode ifAbsent: [ ^self ]) uninstall