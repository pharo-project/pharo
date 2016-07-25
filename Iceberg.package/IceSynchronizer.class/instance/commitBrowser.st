accessing
commitBrowser
	^ commitBrowser ifNil: [ commitBrowser := IceCommitBrowser new ]