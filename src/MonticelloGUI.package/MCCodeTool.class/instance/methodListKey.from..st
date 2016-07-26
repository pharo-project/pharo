menus
methodListKey: aKeystroke from: aListMorph 
	aKeystroke caseOf: {
		[$b] -> [self browseMethodFull].
		[$h] -> [self classHierarchy].
		[$o] -> [self fileOutMessage].
		[$c] -> [self copySelector].
		[$n] -> [self browseSendersOfMessages].
		[$m] -> [self browseMessages].
		[$i] -> [self methodHierarchy].
		[$v] -> [self browseVersions]}
		 otherwise: []