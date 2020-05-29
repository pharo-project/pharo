"
I implement the select: operation on the iterator.
I apply the block and filter the elements returned by myself.

"
Class {
	#name : #SptFilter,
	#superclass : #SptIteratorBlockDecorator,
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #initialization }
SptFilter >> filter: aValue [

	^ aBlock value: aValue
]

{ #category : #accessing }
SptFilter >> next [

	| value |

	[ inner atEnd ]
		whileFalse: [ value := inner next.
			(self filter: value)
				ifTrue: [ ^ value ] ].
	^ nil
]
