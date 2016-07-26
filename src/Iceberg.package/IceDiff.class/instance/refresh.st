actions
refresh
	elements ifNotNil: [ elements do: #refresh ].
	changes := nil.
	announcer announce: IceChangeSetChanged new.
