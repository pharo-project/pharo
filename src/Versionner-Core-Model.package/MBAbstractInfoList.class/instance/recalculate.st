updating
recalculate

	text := groups := validationResult := validationIcon := projects:= packages := nil.
	infoList == nil ifTrue: [ ^self ].
	self announcer announce: (MBInfoListChanged changed: self).
	infoList := nil