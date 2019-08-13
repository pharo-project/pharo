Class {
	#name : #DeprecatedClassForTest,
	#superclass : #Object,
	#category : #'Kernel-Tests-Extended-Methods'
}

{ #category : #deprecation }
DeprecatedClassForTest class >> isDeprecated [
	^ true
]

{ #category : #examples }
DeprecatedClassForTest >> deprecatedMethod [
	self deprecated: 'I am deprecated for tests'
]

{ #category : #examples }
DeprecatedClassForTest >> indirectDeprecatedMethod [
	
]
