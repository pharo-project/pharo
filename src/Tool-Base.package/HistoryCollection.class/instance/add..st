protocol
add: anElement

	| count |
	count := 0.
	(self includes: anElement)
		ifTrue: [ | association |
			association := self remove: anElement.
			count := association value ].
	self size = self maxSize
		ifTrue: [	self removeLast ].

	^ storage addFirst: (anElement -> (count+1))