accessing
entriesMatching: prefix
	| list |
	
	list := self listForPrefix: prefix.
	^ list entriesMatching: prefix