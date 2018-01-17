"
I am a raw data layout that holds bytes (8 bits).
"
Class {
	#name : #ByteLayout,
	#superclass : #BitsLayout,
	#category : #'Slot-Layout'
}

{ #category : #'instance creation' }
ByteLayout class >> extending: superLayout scope: aScope host: aClass [
	^ superLayout extendByte
		host: aClass;
		yourself
]

{ #category : #extending }
ByteLayout >> extendByte [
	^ ByteLayout new
]

{ #category : #format }
ByteLayout >> instanceSpecification [
	^ 16
]

{ #category : #testing }
ByteLayout >> isBytes [
	^ true
]
