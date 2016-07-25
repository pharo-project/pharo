execute
execute
	Smalltalk at: #HelpBrowser ifPresent: [ :class | ^class openOn: (Smalltalk at: #VersionnerHelp) ].
	self workspaceHelp