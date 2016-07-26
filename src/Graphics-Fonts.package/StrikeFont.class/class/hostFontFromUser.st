font creation
hostFontFromUser
	"StrikeFont hostFontFromUser"
	| fontNames index labels |
	fontNames := self listFontNames asSortedCollection.
	labels := (String new: 100) writeStream.
	fontNames 
		do: [ :fn | labels nextPutAll: fn ]
		separatedBy: [ labels cr ].
	index := UIManager default 
		chooseFrom: labels contents substrings
		title: 'Choose your font'.
	index = 0 ifTrue: [ ^ nil ].
	^ fontNames at: index