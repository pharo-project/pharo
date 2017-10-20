running
setUp
	keys := (1 to: 1000) collect:[:n| 'key', n asString].
	dict := WeakKeyDictionary new.
	keys do:[:n| dict at: n put: n,n].
	super setUp.