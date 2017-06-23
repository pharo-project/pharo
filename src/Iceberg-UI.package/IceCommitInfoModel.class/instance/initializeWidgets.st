initialization
initializeWidgets
	idLabel := self newLabel.
	idPanel := self newTextInput enabled: false.
	dateLabel := self newLabel.
	datePanel := self newTextInput enabled: false.
	authorLabel := self newLabel.
	authorPanel := self newTextInput enabled: false.
	commentLabel := self newLabel.
	commentPanel := self newText enabled: false.
	parentsLabel := self newLabel.
	parentsPanel := self newTextInput enabled: false.
	
	idLabel label: 'Commit Id'.
	idPanel text: self commit id asString, ' [', self commit shortId, ']'.
	dateLabel label: 'Date & time'.
	datePanel text: self commit datetime asLocalStringYMDHM.
	authorLabel label: 'Author'.
	authorPanel text: self commit username.
	commentLabel label: 'Comment'.
	commentPanel text: self commit comment.
	parentsLabel label: 'Parent(s)'.
	parentsPanel text: (self commit parents collect: #shortId) asCommaString
	