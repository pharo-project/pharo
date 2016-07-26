accessing
next
	| dummy tail |

	dummy := self newItem.
	dummy object: dummy.

	"this is atomic"
	tail := head.
	head := dummy.

	"skip over dummies"
	[ tail object == tail ] whileTrue: [ 
		[ tail isCircular ] whileTrue: [ self yield ].
		(tail := tail next) ifNil: [  | result |
			"queue is empty. block until new items appear"
			head == dummy ifTrue: [ self signalNoMoreItems ].
			[ head == dummy ] whileTrue: [ self waitForNewItems ].
			dummy next: nil. 
			result := self next.
			^ result ] ].

	dummy next: tail next.

	^ tail object
	