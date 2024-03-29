"
Rewrite super messages to self messages when both refer to same method
"
Class {
	#name : 'ReSuperSendsRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReSuperSendsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'SuperSendsRule'
]

{ #category : 'accessing' }
ReSuperSendsRule >> group [
	^ 'Design Flaws'
]

{ #category : 'initialization' }
ReSuperSendsRule >> initialize [
	super initialize.
	self
		replace: 'super `@message: ``@args'
		with: 'self `@message: ``@args'
]

{ #category : 'accessing' }
ReSuperSendsRule >> name [
	^ 'Rewrite super messages to self messages'
]

{ #category : 'testing' }
ReSuperSendsRule >> shouldCheckMethod: aMethod [

	^ aMethod methodClass withAllSubclasses noneSatisfy: [ :class | class includesSelector: aMethod selector ]
]
