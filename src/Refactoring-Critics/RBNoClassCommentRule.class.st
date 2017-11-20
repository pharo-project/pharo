"
This smell arises when a class has no comment. Classes should have comments to explain their purpose, collaborations with other classes, and optionally provide examples of use.
"
Class {
	#name : #RBNoClassCommentRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
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
	(aClass isMeta or: [ aClass isTestCase ])
		ifTrue: [ ^ false ].
	^ aClass hasComment not
]

{ #category : #running }
RBNoClassCommentRule >> checkClass: aClass [
	(self basicCheck: aClass)
		ifTrue: [ result addClassAndMetaClassFor: aClass ]
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
