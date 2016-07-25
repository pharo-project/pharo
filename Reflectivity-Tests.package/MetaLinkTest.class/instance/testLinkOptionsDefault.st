tests - options
testLinkOptionsDefault
	| link |
	link := MetaLink new.

	self assert: link optionInlineMetaObject.
	self deny: link optionCompileOnLinkInstallation.