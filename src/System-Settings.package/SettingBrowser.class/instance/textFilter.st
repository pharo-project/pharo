updating
textFilter
	^ self regexpSearch
		ifFalse: [SettingTokenFilter new token: self searchedText]
		ifTrue: [SettingRegexpFilter new regexp: self searchedText] 