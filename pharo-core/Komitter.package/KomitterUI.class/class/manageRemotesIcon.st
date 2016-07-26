icons
manageRemotesIcon

	^ manageRemotesIcon ifNil: [ manageRemotesIcon := Smalltalk ui icons form16x16FromContents: self manageRemotesData ]