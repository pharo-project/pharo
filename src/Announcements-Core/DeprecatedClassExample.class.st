Class {
	#name : #DeprecatedClassExample,
	#superclass : #Object,
	#category : #'Announcements-Core'
}

{ #category : #testing }
DeprecatedClassExample class >> isDeprecated [ 
	^true
]

{ #category : #deprecation }
DeprecatedClassExample class >> readDeprecatedValue [

	self
		deprecated: 'Please use #crTrace678 instead'
		transformWith: 'DeprecatedClassExample' -> 'Object'.

	^self
]
