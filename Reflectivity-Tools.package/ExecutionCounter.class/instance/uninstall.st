add/remove
uninstall
	node hasExecutionCounter ifFalse: [ ^self ].
	link uninstall.
	self class allCounters removeKey: node.