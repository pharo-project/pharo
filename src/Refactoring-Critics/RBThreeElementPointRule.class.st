"
Checks arithmetic statements for possible three element points (i.e., a point that has another point in its x or y part).
"
Class {
	#name : #RBThreeElementPointRule,
	#superclass : #RBParseTreeLintRule,
	#category : #Refactoring-Critics-ParseTreeRules
}

{ #category : #accessing }
RBThreeElementPointRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ThreeElementPointRule'
]

{ #category : #accessing }
RBThreeElementPointRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBThreeElementPointRule >> initialize [
	super initialize.
	self matcher
		matches: '``@x @ ``@y'
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					| current |
					current := node parent.
					[ current isNil or: [ current isMessage and: [ current selector = #@ or: [ current selector isInfix not ] ] ] ] 
						whileFalse: [ current := current parent ].
					(current isNil or: [ current isMessage and: [ current selector isInfix not ] ]) 
						ifTrue: [ nil ]
						ifFalse: [ node ] ]
				ifFalse: [ answer ] ]
]

{ #category : #accessing }
RBThreeElementPointRule >> name [
	^ 'Possible three element point (e.g., x @ y + q @ r)'
]

{ #category : #accessing }
RBThreeElementPointRule >> rationale [
	^ 'Checks arithmetic statements for possible three element points (i.e., a point that has another point in its x or y part).'
]
