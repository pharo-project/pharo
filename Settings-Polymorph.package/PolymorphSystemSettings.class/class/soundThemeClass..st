sound
soundThemeClass: aSoundThemeClass
	aSoundThemeClass ifNil: [^ self].
	self soundThemeClass ~= aSoundThemeClass
		ifTrue: [aSoundThemeClass beCurrent]