helpers
xdgParseUserDirLine: aStream
	"Format is XDG_xxx_DIR=""$HOME/yyy"", where yyy is a shell-escaped homedir-relative path, or XDG_xxx_DIR=""/yyy"", where /yyy is an absolute path. No other format is supported."
	| path firstChar |
	(aStream next = $") ifFalse: [ ^ nil ].
	firstChar := aStream next.
	(#($$ $/) includes: firstChar) ifFalse: [ ^ nil ].
	path := firstChar = $$
				ifTrue: [ (aStream next: 5) = 'HOME/' ifFalse: [ ^ nil ].
					       self home / (aStream upTo: $") ]
				ifFalse: [ self resolveString: '/', (aStream upTo: $") ].
	^ path