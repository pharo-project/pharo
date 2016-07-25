private
browse

	self hasParentNode ifFalse: [ ^ super browseClass ].
	^ self browseClass