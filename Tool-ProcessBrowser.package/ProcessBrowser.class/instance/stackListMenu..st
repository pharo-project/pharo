stack list
stackListMenu: aMenu 
	selectedContext ifNil: [^ aMenu].
	^aMenu addAllFromPragma: 'processBrowserStackListMenu' target: self