initialization
initialize

	super initialize.
	groups := OrderedCollection new asValueHolder.
	title := nil asValueHolder.
	icon := nil asValueHolder.
	autoRefresh := nil asValueHolder.
	autoRefresh whenChangedDo: [ :aBoolean | self menuGroups do: [ :each | each autoRefresh: aBoolean ] ]