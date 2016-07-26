protocol
addToList: aString
	aString isEmpty ifTrue: [ ^ self ].
	list ifNil: [
		list := self searchList].
	list remove: aString ifAbsent: [].
	list size = self maxSize
		ifTrue: [ self searchList removeLast].
	list addFirst: aString.
	self changed: #list.