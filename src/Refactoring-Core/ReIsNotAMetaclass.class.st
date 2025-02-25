Class {
	#name : 'ReIsNotAMetaclass',
	#superclass : 'ReClassCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReIsNotAMetaclass >> check [
	^ class isMeta not
]

{ #category : 'displaying' }
ReIsNotAMetaclass >> violationMessageOn: aWriteStream [ 
	
	aWriteStream nextPutAll: 'Class ', class name , ' is a metaclass'
]

{ #category : 'checking' }
ReIsNotAMetaclass >> violators [
	^ { class }
]
