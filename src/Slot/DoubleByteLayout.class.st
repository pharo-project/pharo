"
I am a raw data layout that holds double bytes (16 bits).
"
Class {
	#name : #DoubleByteLayout,
	#superclass : #BitsLayout,
	#category : #'Slot-Layout'
}

{ #category : #'instance creation' }
DoubleByteLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendDoubleByte
		host: aClass;
		yourself
]

{ #category : #extending }
DoubleByteLayout >> extendDoubleByte [
	^ DoubleByteLayout new
]

{ #category : #format }
DoubleByteLayout >> instanceSpecification [
	^ 12
]

{ #category : #testing }
DoubleByteLayout >> isDoubleBytes [
	^ true
]
