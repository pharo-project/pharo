"
This smell arises when a class has no comment. Classes should have comments to explain their purpose, collaborations with other classes, and optionally provide examples of use.
"
Class {
	#name : #RBNoClassCommentRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBNoClassCommentRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBNoClassCommentRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'NoClassCommentRule'
]

{ #category : #running }
RBNoClassCommentRule >> basicCheck: aClass [
	(aClass isMeta or: [ aClass isTestCase ]) ifTrue: [ ^ false ].
	^ aClass hasComment not
]

{ #category : #accessing }
RBNoClassCommentRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBNoClassCommentRule >> name [
	^ 'No class comment'
]

{ #category : #accessing }
RBNoClassCommentRule >> rationale [
	^ 'Classes should have comments to explain their purpose, collaborations with other classes, and optionally provide examples of use.'
]

{ #category : #accessing }
RBNoClassCommentRule >> resultClass [
	^ RBClassEnvironment
]
