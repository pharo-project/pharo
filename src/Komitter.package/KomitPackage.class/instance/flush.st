protocol
flush
	super flush.
	
	classes ifNotNil: [ classes do: [ :each | each flush ] ].
	classes := nil.
	isFullyCommited := true