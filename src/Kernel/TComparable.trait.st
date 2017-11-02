"
This trait is a protocol for comparable objects. Classes using it must implement #<, #=, and #hash.
"
Trait {
	#name : #TComparable,
	#category : 'Kernel-Numbers'
}

{ #category : #comparing }
TComparable >> < aComparable [
	"Answer whether the receiver is less than the argument."

	^self subclassResponsibility
]

{ #category : #comparing }
TComparable >> <= aComparable [ 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aComparable) not
]

{ #category : #comparing }
TComparable >> = aComparable [ 
	"Compare the receiver with the argument and answer with true if the 
	receiver is equal to the argument. Otherwise answer false."

	^self subclassResponsibility
]

{ #category : #comparing }
TComparable >> > aComparable [ 
	"Answer whether the receiver is greater than the argument."

	^aComparable < self
]

{ #category : #comparing }
TComparable >> >= aComparable [ 
	"Answer whether the receiver is greater than or equal to the argument."

	^aComparable <= self
]

{ #category : #comparing }
TComparable >> between: min and: max [ 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]
]

{ #category : #comparing }
TComparable >> hash [
	"Hash must be redefined whenever = is redefined."

	^self subclassResponsibility
]

{ #category : #comparing }
TComparable >> max: aComparable [ 
	"Answer the receiver or the argument, whichever is greater."

	self > aComparable
		ifTrue: [^self]
		ifFalse: [^aComparable]
]

{ #category : #comparing }
TComparable >> min: aComparable [ 
	"Answer the receiver or the argument, whichever is lesser."

	self < aComparable
		ifTrue: [^self]
		ifFalse: [^aComparable]
]

{ #category : #comparing }
TComparable >> min: aMin max: aMax [ 

	^ (self min: aMin) max: aMax
]
