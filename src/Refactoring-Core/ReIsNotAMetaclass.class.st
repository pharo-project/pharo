Class {
	#name : 'ReIsNotAMetaclass',
	#superclass : 'ReClassCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReIsNotAMetaclass >> check [
	^ aClass isMeta not
]

{ #category : 'checking' }
ReIsNotAMetaclass >> violators [
	^ { aClass }
]
