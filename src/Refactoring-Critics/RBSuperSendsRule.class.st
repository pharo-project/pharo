"
Rewrite super messages to self messages when both refer to same method
"
Class {
	#name : #RBSuperSendsRule,
	#superclass : #RBTransformationRule,
	#category : 'Refactoring-Critics-TransformationRules'
}

{ #category : #accessing }
RBSuperSendsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SuperSendsRule'
]

{ #category : #accessing }
RBSuperSendsRule >> group [
	^ 'Design Flaws'
]

{ #category : #initialization }
RBSuperSendsRule >> initialize [
	super initialize.
	self rewriteRule 
		replace: 'super `@message: ``@args' 
		with: 'self `@message: ``@args'
		when: [ :node | 
			(class withAllSubclasses 
				detect: [:each | each includesSelector: node selector]
				ifNone: [ nil ]) isNil ]
]

{ #category : #accessing }
RBSuperSendsRule >> name [
	^ 'Rewrite super messages to self messages'
]
