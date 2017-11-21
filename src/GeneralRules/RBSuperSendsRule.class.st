"
Rewrite super messages to self messages when both refer to same method
"
Class {
	#name : #RBSuperSendsRule,
	#superclass : #ReNodeRewriteRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBSuperSendsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SuperSendsRule'
]

{ #category : #hooks }
RBSuperSendsRule >> afterCheck: aNode mappings: mappingDict [
		
	aNode methodNode ifNotNil: [ :methNode |
		methNode compiledMethod ifNotNil: [ :method |
			method methodClass withAllSubclassesDo: [ :class |
			 (class includesSelector: aNode selector)
				ifTrue: [ ^ false ] ]] ].
	^ true
]

{ #category : #accessing }
RBSuperSendsRule >> group [
	^ 'Design Flaws'
]

{ #category : #initialization }
RBSuperSendsRule >> initialize [
	super initialize.
	self 
		replace: 'super `@message: ``@args' 
		with: 'self `@message: ``@args'
		
]

{ #category : #accessing }
RBSuperSendsRule >> name [
	^ 'Rewrite super messages to self messages'
]
