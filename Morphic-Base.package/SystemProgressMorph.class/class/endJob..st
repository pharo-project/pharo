job subscription
endJob: aJobEnd 
	| bars |
	self flag: #toFix. "camillo I need help"
	bars := self uniqueInstance bars.
	bars isEmpty
		ifFalse: [ self uniqueInstance close:  bars last ] 