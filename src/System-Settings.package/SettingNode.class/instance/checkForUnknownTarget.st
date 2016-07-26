checking
checkForUnknownTarget
	self parentNode ifNotNil: [:p | item target ifNil: [item target: p item target].
		item targetSelector ifNil: [item targetSelector: p item targetSelector]].
	self allChildren do: [:c | c checkForUnknownTarget]