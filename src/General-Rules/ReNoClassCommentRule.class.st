"
This smell arises when a class has no comment. Classes should have comments to explain their purpose, collaborations with other classes, and optionally provide examples of use.
"
Class {
	#name : 'ReNoClassCommentRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'testing' }
ReNoClassCommentRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReNoClassCommentRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReNoClassCommentRule class >> rationale [
	^ 'Classes should have comments to explain their purpose, collaborations with other classes, and optionally provide examples of use.'
]

{ #category : 'accessing' }
ReNoClassCommentRule class >> ruleName [
	^ 'No class comment'
]

{ #category : 'accessing' }
ReNoClassCommentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'NoClassCommentRule'
]

{ #category : 'running' }
ReNoClassCommentRule >> basicCheck: aClass [
	(aClass isMeta or: [ aClass isTestCase ]) ifTrue: [ ^ false ].
	^ aClass hasComment not
]
