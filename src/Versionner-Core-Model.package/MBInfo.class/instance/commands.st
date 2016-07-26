commands
commands
	^ (self class allSelectors select:
		[ :k | k beginsWith: 'cmd'  ]) collect: [ :k | self perform: k ]