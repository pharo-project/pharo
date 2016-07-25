protocol
flush
	super flush.
	definition := nil.
	operations removeAll.
	methods ifNotNil: [ :m | m do: [ :e | e flush ] ].
	methods := nil.
	definitions ifNotNil: [ :d | d do: [ :e | e flush ] ].
	definitions := nil