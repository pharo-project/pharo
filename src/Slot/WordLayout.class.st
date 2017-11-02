"
I am a raw data layout that holds words (32 / 64 bit depending on the platform).
"
Class {
	#name : #WordLayout,
	#superclass : #BitsLayout,
	#category : 'Slot-Layout'
}

{ #category : #'instance creation' }
WordLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendWord
		host: aClass;
		yourself
]

{ #category : #extending }
WordLayout >> extendByte [
	IncompatibleLayoutConflict new
		layout: self;
		subType: #byte;
		signal
]

{ #category : #format }
WordLayout >> instanceSpecification [
	^ 10
]

{ #category : #testing }
WordLayout >> isBytes [
	^ false
]
