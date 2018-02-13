"
I am a raw data layout that holds double words (64 bit).
"
Class {
	#name : #DoubleWordLayout,
	#superclass : #BitsLayout,
	#category : #'Slot-Layout'
}

{ #category : #extending }
DoubleWordLayout >> extendDoubleWord [
	^ DoubleWordLayout new
]

{ #category : #'methodsFor:' }
DoubleWordLayout >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendDoubleWord
		host: aClass;
		yourself
]

{ #category : #format }
DoubleWordLayout >> instanceSpecification [
	^ 9
]

{ #category : #testing }
DoubleWordLayout >> isDoubleWords [
	^ true
]
