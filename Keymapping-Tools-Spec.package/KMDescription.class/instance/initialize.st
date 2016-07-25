initialization
initialize

	categories := nil asValueHolder.
	sortingBlock := [:a :b | a first < b first ].
	super initialize.
      self collectShortcutDescription
