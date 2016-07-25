instance creation
fromUnixTime: anInteger
	^ self fromSeconds: anInteger + 2177452800 "unix epoch constant"