private
cleanupConnection
	(self isOneShot or: [ request notNil and: [ request wantsConnectionClose ] ])
		ifTrue: [
			self close ]