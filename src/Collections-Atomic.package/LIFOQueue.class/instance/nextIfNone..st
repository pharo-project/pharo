accessing
nextIfNone: aBlock
	| dummy tail |

	dummy := self newItem.
	dummy object: dummy.

	tail := head.
	head := dummy.

	"skip over dummies"
	[ tail object == tail ] whileTrue: [ 
		[ tail isCircular ] whileTrue: [ self yield ].
		(tail := tail next) ifNil: [ 
			dummy next: nil. 
			dummy == head ifTrue: [ self signalNoMoreItems].
			^ aBlock value ] ].

	dummy next: tail next.

	^ tail object
	