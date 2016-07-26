parsing
parseAuthority: string from: start to: stop
	| index |
	((index := string indexOf: $@ startingAt: start) > 0 and: [ index < stop ])
		ifTrue: [
			self parseUserInfo: (ReadStream on: string from: start to: index - 1).
			self parseHostPort: (ReadStream on: string from: index + 1 to: stop) ]
		ifFalse: [ 
			self parseHostPort: (ReadStream on: string from: start to: stop) ]