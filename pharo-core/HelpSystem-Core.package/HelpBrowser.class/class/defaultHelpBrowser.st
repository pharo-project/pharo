accessing
defaultHelpBrowser
	^ DefaultHelpBrowser ifNil: [DefaultHelpBrowser := self]
	