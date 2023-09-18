"
Layout scopes reify how classes extend the layout of their superclass.
"
Class {
	#name : 'AbstractLayoutScope',
	#superclass : 'Object',
	#category : 'Kernel-Layout',
	#package : 'Kernel',
	#tag : 'Layout'
}

{ #category : 'testing' }
AbstractLayoutScope class >> isAbstract [

	^ self == AbstractLayoutScope
]

{ #category : 'comparing' }
AbstractLayoutScope >> = other [
	^ self class = other class
]

{ #category : 'enumerating' }
AbstractLayoutScope >> allSlotsDo: aBlock [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractLayoutScope >> allVisibleSlots [
	^ self subclassResponsibility
]

{ #category : 'extending' }
AbstractLayoutScope >> extend [
	^ self extend: { }
]

{ #category : 'extending' }
AbstractLayoutScope >> extend: someSlots [
	^ self extend: someSlots as: LayoutClassScope
]

{ #category : 'extending' }
AbstractLayoutScope >> extend: someSlots as: type [
	| scope fieldIndex  |
	scope := type new: someSlots size.
	fieldIndex := self firstFieldIndex.
	scope parentScope: self.

	someSlots withIndexDo: [ :slot :slotIndex | |realSlot|
		realSlot := slot asSlot.
		realSlot scope: scope.
		realSlot isVirtual ifFalse: [realSlot index: fieldIndex].
		fieldIndex := fieldIndex + realSlot size.
		scope at: slotIndex put: realSlot ].
	^ scope
]

{ #category : 'accessing' }
AbstractLayoutScope >> fieldSize [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractLayoutScope >> firstFieldIndex [
	^ self fieldSize + 1
]

{ #category : 'flattening' }
AbstractLayoutScope >> flatten [

	^ self flattenIn: OrderedCollection new
]

{ #category : 'flattening' }
AbstractLayoutScope >> flattenIn: aCollection [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractLayoutScope >> hasBindingThatBeginsWith: arg1 [
	^ self subclassResponsibility
]

{ #category : 'testing' }
AbstractLayoutScope >> hasFields [
	self subclassResponsibility
]

{ #category : 'testing' }
AbstractLayoutScope >> hasSlots [
	self subclassResponsibility
]

{ #category : 'comparing' }
AbstractLayoutScope >> hash [
	^ self class hash
]

{ #category : 'testing' }
AbstractLayoutScope >> ifNotEmpty: aBlock [
	self subclassResponsibility
]

{ #category : 'enumerating' }
AbstractLayoutScope >> indexOf: anElement [
	"Answer the index of the first occurrence of anElement within the
	receiver. If the receiver does not contain anElement, answer 0."

	^ self indexOf: anElement ifAbsent: 0
]

{ #category : 'enumerating' }
AbstractLayoutScope >> indexOf: anElement ifAbsent: exceptionBlock [
	"Answer the index of the first occurrence of anElement within the
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	1 to: self size do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
AbstractLayoutScope >> ownFieldSize [
	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractLayoutScope >> resolveSlot: aName [
	^ self
		resolveSlot: aName
		ifFound: [:slot | slot ]
		ifNone: [ SlotNotFound signalForName: aName ]
]

{ #category : 'accessing' }
AbstractLayoutScope >> resolveSlot: aName ifFound: foundBlock ifNone: exceptionBlock [
	self allSlotsDo: [ :slot |
		slot name = aName ifTrue: [ ^ foundBlock cull: slot ]].
	^ exceptionBlock value
]

{ #category : 'accessing' }
AbstractLayoutScope >> visibleSlotNames [
	^self visibleSlots collect: [:each | each name ]
]

{ #category : 'accessing' }
AbstractLayoutScope >> visibleSlots [
	self subclassResponsibility
]
