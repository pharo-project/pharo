initialization
initialize

	super initialize.
	
	self children: [ #() ].
	self whenSelectedChanged: [ :aBoolean | self parentNode ifNotNil: [ :parent | parent checkChildren ] ]