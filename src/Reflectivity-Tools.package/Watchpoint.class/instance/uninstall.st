install
uninstall
	node hasWatchpoint ifFalse: [ ^self ].
	link uninstall.
	self class allWatchpoints removeKey: node.