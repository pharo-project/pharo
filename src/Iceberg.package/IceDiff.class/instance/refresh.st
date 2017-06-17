actions
refresh
	self repository refresh.
	elements ifNotNil: [ elements do: #refresh ].
	announcer announce: IceChangeSetChanged new.
