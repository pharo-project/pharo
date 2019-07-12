"
Each rule should define a #basicCheck: method which accepts an entity to be validated by the rule.

If the entity violates the rule, method should ruturn ""true"", otherwise - ""false"".

It is recommended to reuse #basicCheck: functionality in #checkClass: and #checkMethod:
"
Class {
	#name : #RBDefineBasicCheckRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBDefineBasicCheckRule class >> checksClass [
	^ true
]

{ #category : #running }
RBDefineBasicCheckRule >> basicCheck: aClass [
	^ (aClass inheritsFrom: RBLintRule) and: [ aClass isVisible and: [ (aClass lookupSelector: #basicCheck:) isSubclassResponsibility ] ]
]

{ #category : #helpers }
RBDefineBasicCheckRule >> critiqueFor: aClass [
	^ (ReMissingMethodCritique
		for: aClass
		by: self
		class: aClass
		selector: #basicCheck:) beShouldBeImplemented
]

{ #category : #accessing }
RBDefineBasicCheckRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBDefineBasicCheckRule >> name [
	^ 'Rule does not define #basicCheck:'
]
