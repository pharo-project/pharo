"
Empty top-level layout used to delimit the layout chains.
"
Class {
	#name : #EmptyLayout,
	#superclass : #AbstractLayout,
	#classInstVars : [
		'instance'
	],
	#category : #'Slot-Layout'
}

{ #category : #'instance creation' }
EmptyLayout class >> instance [
	^ instance ifNil: [ instance := self new ]
]

{ #category : #extending }
EmptyLayout >> extend: someSlots [
	^ FixedLayout new
		slotScope: (LayoutEmptyScope instance extend: someSlots)
]

{ #category : #extending }
EmptyLayout >> extendByte [
	^ ByteLayout new
]

{ #category : #extending }
EmptyLayout >> extendDoubleByte [
	^ DoubleByteLayout new
]

{ #category : #extending }
EmptyLayout >> extendDoubleWord [
	^ DoubleWordLayout new
]

{ #category : #extending }
EmptyLayout >> extendVariable: someSlots [
	^ VariableLayout new
		slotScope: (LayoutEmptyScope extend: someSlots)
]

{ #category : #extending }
EmptyLayout >> extendWeak [
	^ WeakLayout new
]

{ #category : #extending }
EmptyLayout >> extendWord [
	^ WordLayout new
]
