actions
refresh
	elements ifNotNil: [ elements do: #refresh ].
	elements := nil.
	announcer announce: IceChangeSetChanged new.
