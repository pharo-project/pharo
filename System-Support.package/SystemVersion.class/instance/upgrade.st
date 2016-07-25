upgrading
upgrade

	| parts |
	version ifNil: [^self].
	parts := self class parseShortVersionString: version.
	self type: parts first.
	self major: parts second.
	self minor: parts third.
	self suffix: parts fourth.
	version := nil.