converting
asUnixTime
	"answer number of seconds since unix epoch (midnight Jan 1, 1970, UTC)"
	^((self offset: Duration zero) - self class unixEpoch) asSeconds