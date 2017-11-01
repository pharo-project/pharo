"
I am a raw data layout that holds bytes (8 bit).
"
Class {
	#name : #ByteLayout,
	#superclass : #BitsLayout,
	#category : 'Slot-Layout'
}

{ #category : #'instance creation' }
ByteLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendByte
		host: aClass;
		yourself
]

{ #category : #extending }
ByteLayout >> extendWord [
	IncompatibleLayoutConflict new
		layout: self;
		subType: #word;
		signal
]

{ #category : #format }
ByteLayout >> instanceSpecification [
	^ 16
]

{ #category : #testing }
ByteLayout >> isBytes [
	^ true
]
