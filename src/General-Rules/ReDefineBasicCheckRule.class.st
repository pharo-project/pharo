"
Each rule should define a #basicCheck: method which accepts an entity to be validated by the rule.

If the entity violates the rule, method should ruturn ""true"", otherwise - ""false"".

It is recommended to reuse #basicCheck: functionality in #checkClass: and #checkMethod:
"
Class {
	#name : 'ReDefineBasicCheckRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'testing' }
ReDefineBasicCheckRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReDefineBasicCheckRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReDefineBasicCheckRule class >> ruleName [
	^ 'Rule does not define #basicCheck:'
]

{ #category : 'running' }
ReDefineBasicCheckRule >> basicCheck: aClass [
	^ (aClass inheritsFrom: RBLintRule) and: [ aClass isVisible and: [ (aClass lookupSelector: #basicCheck:) isSubclassResponsibility ] ]
]

{ #category : 'helpers' }
ReDefineBasicCheckRule >> critiqueFor: aClass [
	^ (ReMissingMethodCritique
		for: aClass
		by: self
		class: aClass
		selector: #basicCheck:) beShouldBeImplemented
]
