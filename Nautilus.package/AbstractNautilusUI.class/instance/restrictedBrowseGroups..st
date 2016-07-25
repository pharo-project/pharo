menus behavior
restrictedBrowseGroups: aCollection

	aCollection
		ifNotEmpty: [:groups || classes newEnvironment |
			classes := groups flatCollect: [:group | group classes].
			newEnvironment := self browsedEnvironment forClasses: classes.
			self model class openOnGroup: groups first inEnvironment: newEnvironment ]