updating
updateAccordingTo: aFilter 
	(aFilter keepSettingTreeNode: self) 
		ifFalse: [
			self contents do: [ :n | n updateAccordingTo: aFilter].
			self contents removeAllSuchThat: [ :n | (aFilter keepSettingTreeNode: n) not  and: [n isEmpty]]]
		