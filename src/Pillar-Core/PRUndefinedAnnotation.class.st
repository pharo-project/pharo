Class {
	#name : #PRUndefinedAnnotation,
	#superclass : #PRAbstractAnnotation,
	#instVars : [
		'tag'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRUndefinedAnnotation class >> acceptName: aTag [ 
	^ false
]

{ #category : #protected }
PRUndefinedAnnotation class >> tag [
	^ 'undefined'
]

{ #category : #'instance creation' }
PRUndefinedAnnotation class >> withParameters: aPRParameters [
	^ self new withParameters: aPRParameters
]

{ #category : #comparing }
PRUndefinedAnnotation >> = anObject [
	^ super = anObject and: [
		self tag = anObject tag ]
]

{ #category : #visiting }
PRUndefinedAnnotation >> accept: aVisitor [
	^ aVisitor visitUndefinedAnnotation: self
]

{ #category : #comparing }
PRUndefinedAnnotation >> hash [
	^ super hash bitXor: self tag hash
]

{ #category : #initialization }
PRUndefinedAnnotation >> initialize [
	super initialize.
	hadAllKeys := true.
	tag := self class tag.
]

{ #category : #accessing }
PRUndefinedAnnotation >> tag [
	^ tag
]

{ #category : #accessing }
PRUndefinedAnnotation >> tag: anObject [
	tag := anObject
]

{ #category : #initialization }
PRUndefinedAnnotation >> withParameters: aPRParameters [ 
	self add: aPRParameters.
	self parameters: aPRParameters asDictionary
]
