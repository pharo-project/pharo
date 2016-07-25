ston persistence
fromSton: stonReader

	stonReader parseListDo: [ :each :index | "nothing to read" ].

	^ self uniqueInstance