accessing
add: aCookie
	(self removeIfExpired: aCookie) ifTrue: [ ^ nil ].
	self cookies do: [ :cookie |
		cookie = aCookie ifTrue: [ self cookies remove: cookie ] ].
	self cookies add: aCookie
	