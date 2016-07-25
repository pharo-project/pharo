private
pickAncestorVersionInfo
	| ancestors |
	ancestors := target workingCopy ancestry breadthFirstAncestors.
	^self uiManager 
		chooseFrom: (ancestors collect: [:ea | ea name])
		values: ancestors
		title: 'Package:' 
