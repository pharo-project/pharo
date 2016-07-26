accessing
expire
    "Release the value if the key has gone. Return true if a modification was made."
	(self basicAt: 1) ifNil:[
		value == self ifTrue:[^false].
		value := self.
		^true
	].
	^false