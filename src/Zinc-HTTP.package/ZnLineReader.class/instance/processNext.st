private
processNext
	| item |
	stream atEnd ifTrue: [ ^ nil ].
	(item := stream next) ifNil: [ ^ nil ].
	stream isBinary ifTrue: [ item := item asCharacter ].
	item == Character lf
		ifTrue: [
			(position > 0 and: [ (buffer at: position) == Character cr ])
				ifTrue: [
					position := position - 1.
					^ nil ] ].
	self store: item.
	^ item
			
	