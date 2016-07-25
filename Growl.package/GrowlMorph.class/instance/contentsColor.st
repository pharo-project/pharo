accessing
contentsColor
	^ contentsColor ifNil: [ contentsColor := self theme growlContentsColorFor: self ]
