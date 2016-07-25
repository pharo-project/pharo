analyzing
run

	objectStack push: root.
	[ objectStack isEmpty ] 
		whileFalse: [ self mapAndTrace: objectStack pop ].
	[ privateObjectStack isEmpty ] 
		whileFalse: [ self privateMapAndTrace: privateObjectStack pop ].