user interface
issueTitleOn: canvas
	| sliceTitleEntry |
	sliceTitleEntry := canvas window
				newAutoAcceptTextEntryFor: self info
				get: #title
				set: #title:
				class: String
				getEnabled: nil
				help: 'An identification label for the slice. Just copy-paste the Fogbugz issue title here' translated.
	sliceTitleEntry ghostText: 'Just paste the issue title here'.
	^ sliceTitleEntry