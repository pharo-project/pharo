private
addAll: aWindow  withSpec: aSpec

	aWindow
		addMorph: (self buildWithSpec: aSpec)
		fullFrame: ((0@0 corner: 1@0) asSpecLayoutFrame bottomOffset: 30).