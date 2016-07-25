initialization
initialize
	super initialize.
	items := OrderedCollection new asValueHolder.
	autoRefresh := nil asValueHolder.
	autoRefresh whenChangedDo: [ :aBoolean | self menuItems do: [ :each | each autoRefresh: aBoolean ] ]