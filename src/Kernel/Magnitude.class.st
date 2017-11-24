"
I'm the abstract class Magnitude that provides common protocol for objects that have
the ability to be compared along a linear dimension, such as dates or times.
Subclasses of Magnitude include Date, ArithmeticValue, and Time, as well as
Character and LookupKey.
 
 
My subclasses should implement
  < aMagnitude 
  = aMagnitude 
  hash

Here are some example of my protocol:
     3 > 4
     5 = 6
     100 max: 9
	7 between: 5 and: 10 

"
Class {
	#name : #Magnitude,
	#superclass : #Object,
	#category : #'Kernel-Numbers'
}

{ #category : #testing }
Magnitude >> < aMagnitude [ 
	"Answer whether the receiver is less than the argument."

	^self subclassResponsibility
]

{ #category : #testing }
Magnitude >> <= aMagnitude [ 
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aMagnitude) not
]

{ #category : #comparing }
Magnitude >> = aMagnitude [ 
	"Compare the receiver with the argument and answer with true if the 
	receiver is equal to the argument. Otherwise answer false."

	^self subclassResponsibility
]

{ #category : #testing }
Magnitude >> > aMagnitude [ 
	"Answer whether the receiver is greater than the argument."

	^aMagnitude < self
]

{ #category : #testing }
Magnitude >> >= aMagnitude [ 
	"Answer whether the receiver is greater than or equal to the argument."

	^aMagnitude <= self
]

{ #category : #testing }
Magnitude >> between: min and: max [ 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]
]

{ #category : #comparing }
Magnitude >> compareWith: anotherMagnitude ifLesser: lesserBlock ifEqual: equalBlock ifGreater: greaterBlock [

	^ self < anotherMagnitude
		ifTrue: lesserBlock
		ifFalse: [ 
			self = anotherMagnitude
				ifTrue: equalBlock
				ifFalse: greaterBlock ]
]

{ #category : #comparing }
Magnitude >> hash [
	"Hash must be redefined whenever = is redefined."

	^self subclassResponsibility
]

{ #category : #comparing }
Magnitude >> max: aMagnitude [ 
	"Answer the receiver or the argument, whichever has the greater 
	magnitude."

	self > aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]
]

{ #category : #comparing }
Magnitude >> min: aMagnitude [ 
	"Answer the receiver or the argument, whichever has the lesser 
	magnitude."

	self < aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]
]

{ #category : #comparing }
Magnitude >> min: aMin max: aMax [ 

	^ (self min: aMin) max: aMax
]
