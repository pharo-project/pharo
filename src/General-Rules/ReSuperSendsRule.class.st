"
Rewrite super messages to self messages when both refer to same method
"
Class {
	#name : 'ReSuperSendsRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'accessing' }
ReSuperSendsRule class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReSuperSendsRule class >> ruleName [
	^ 'Rewrite super messages to self messages'
]

{ #category : 'accessing' }
ReSuperSendsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'SuperSendsRule'
]

{ #category : 'initialization' }
ReSuperSendsRule >> initialize [
	super initialize.
	self
		replace: 'super `@message: ``@args'
		with: 'self `@message: ``@args'
]

{ #category : 'testing' }
ReSuperSendsRule >> shouldCheckMethod: aMethod [

	^ aMethod methodClass withAllSubclasses noneSatisfy: [ :class | class includesSelector: aMethod selector ]
]
