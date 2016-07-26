updating
updateAccordingTo: aFilter
	self selection: nil.
	self setDescription: ''.
	roots := nil.
	self updateList.
	[
	self rootNodes do: [:r | r updateAccordingTo: aFilter].
	self rootNodes removeAllSuchThat: [:n | (aFilter keepSettingTreeNode: n) not and: [n isEmpty]].
	]
		on: RegexSyntaxError
		do: [:ex | 
			self setDescription: 'Regular expression syntax error, see `documentation'' protocol of RxParser class for user''s guide'].
	self expandAll