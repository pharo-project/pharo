accessing
selectorsFor: aProtocol in: aClass
	^(aClass organization listAtCategoryNamed: aProtocol)
		select: [:each | self includesSelector: each in: aClass]