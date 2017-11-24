"
Each rule should define a #basicCheck: method which accepts an entity to be validated by the rule.

If the entity violates the rule, method should ruturn ""true"", otherwise - ""false"".

It is recommended to reuse #basicCheck: functionality in #checkClass: and #checkMethod:
"
Class {
	#name : #RBDefineBasickCheckRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBDefineBasickCheckRule class >> checksClass [
	^ true
]

{ #category : #'as yet unclassified' }
RBDefineBasickCheckRule >> check: aClass forCritiquesDo: aCritiqueBlock [
	((aClass inheritsFrom: RBLintRule) and: [ 
		aClass isVisible and: [ 
	  (aClass lookupSelector: #basicCheck:) isSubclassResponsibility ] ])

		ifTrue: [ aCritiqueBlock cull: (
			ReMissingMethodCritique
				for: aClass
				by: self
				class: aClass
				selector: #basicCheck:)
				beShouldBeImplemented ]
]

{ #category : #accessing }
RBDefineBasickCheckRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBDefineBasickCheckRule >> name [
	^ 'Rule does not define #basicCheck:'
]
