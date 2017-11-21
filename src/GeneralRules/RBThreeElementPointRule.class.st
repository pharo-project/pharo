"
Checks arithmetic statements for possible three element points (i.e., a point that has another point in its x or y part).
"
Class {
	#name : #RBThreeElementPointRule,
	#superclass : #ReNodeBasedRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBThreeElementPointRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ThreeElementPointRule'
]

{ #category : #running }
RBThreeElementPointRule >> check: aNode forCritiquesDo: aCriticBlock [
	| parentPoint |
	
	aNode isMessage ifFalse: [ ^ self ].
	aNode selector = #@ ifFalse: [ ^ self ].
	
	parentPoint := self
		findSuspiciousParentFor: aNode
		ifNone: [ ^self ].

	aCriticBlock cull: (
		self critiqueFor: parentPoint)		
]

{ #category : #running }
RBThreeElementPointRule >> findSuspiciousParentFor: aNode ifNone: alternativeBlock [
	| tentativeNode |

	tentativeNode := aNode parent.
	tentativeNode ifNil: [ ^ alternativeBlock value ].
	tentativeNode isMessage ifFalse: [
		^ self
			findSuspiciousParentFor: tentativeNode
			ifNone: alternativeBlock ].
	tentativeNode isBinary ifFalse: [ ^ alternativeBlock value ].
	tentativeNode selector = #@ ifTrue: [ ^ tentativeNode ].
	
	^ self
		findSuspiciousParentFor: tentativeNode
		ifNone: alternativeBlock
]

{ #category : #accessing }
RBThreeElementPointRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBThreeElementPointRule >> name [
	^ 'Possible three element point (e.g., x @ y + q @ r)'
]
