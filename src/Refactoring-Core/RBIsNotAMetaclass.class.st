Class {
	#name : 'RBIsNotAMetaclass',
	#superclass : 'RBClassCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBIsNotAMetaclass >> check [
	^ aClass isMeta not
]

{ #category : 'checking' }
RBIsNotAMetaclass >> violators [
	^ { aClass }
]
