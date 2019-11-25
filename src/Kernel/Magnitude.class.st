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

{ #category : #comparing }
Magnitude >> beBetween: minValue and: maxValue [
	"Answer my value constrained to the interval [minValue ; maxValue]
	I take care of the situation where minValue > maxValue"
	"(10 beBetween: 12 and: 20) >>> 12"
	"(10 beBetween: 20 and: 12) >>> 12"
	"(13 beBetween: 12 and: 20) >>> 13"
	"(12 beBetween: 12 and: 20) >>> 12"
	"(100 beBetween: 12 and: 20) >>> 20"
 
	^ minValue <= maxValue 
		ifTrue: [ (self min: maxValue) max: minValue ]
		ifFalse: [ (self min: minValue) max: maxValue ]
]

{ #category : #testing }
Magnitude >> between: min and: max [ 
	"Answer whether the receiver is less than or equal to the argument, max, 
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]
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
Magnitude >> min: maxValue max: minValue [
	"Take the minimum between self and maxValue, then the maximum with minValue"
	"(10 min: 20 max: 5) >>> 10"
	"(10 min: 20 max: 11) >>> 11"
	"(24 min: 20 max: 5) >>> 20"

	^ (self min: maxValue) max: minValue

]
