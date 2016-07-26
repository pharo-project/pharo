private
change

	boolean := boolean not.

	boolean
		 ifTrue: [ self changeToList ]
		 ifFalse: [ self changeToButton ].
		
	self needRebuild: false.
	button needRebuild: false.
	
	self buildWithSpec