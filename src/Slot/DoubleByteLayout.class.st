"
I am a raw data layout that holds double bytes (16 bits).
"
Class {
	#name : #DoubleByteLayout,
	#superclass : #BitsLayout,
	#category : #'Slot-Layout'
}

{ #category : #extending }
DoubleByteLayout >> extendDoubleByte [
	^ DoubleByteLayout new
]

{ #category : #'methodsFor:' }
DoubleByteLayout >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendDoubleByte
		host: aClass;
		yourself
]

{ #category : #format }
DoubleByteLayout >> instanceSpecification [
	^ 12
]

{ #category : #testing }
DoubleByteLayout >> isDoubleBytes [
	^ true
]
