private
loadAncestry
	"Load the ancestry chain to be able to scan it properly."

	| s ancestors |
	s := self gitVersions.
	ancestors := Dictionary new.
	(s match: 'commit ')
		ifFalse: [ self error: 'Unable to find git data in input' ].
	[ s atEnd ]
		whileFalse: [ 
			| a |
			a := (s upToAll: 'commit') substrings collect: [ :e | ByteArray readHexFrom: e trimBoth ].
			ancestors at: a first put: a allButFirst ].
	^ ancestors