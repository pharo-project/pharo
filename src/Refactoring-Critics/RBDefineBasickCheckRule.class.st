"
Each rule should define a #basicCheck: method which accepts an entity to be validated by the rule.

If the entity violates the rule, method should ruturn ""true"", otherwise - ""false"".

It is recommended to reuse #basicCheck: functionality in #checkClass: and #checkMethod:
"
Class {
	#name : #RBDefineBasickCheckRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
}

{ #category : #testing }
RBDefineBasickCheckRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBDefineBasickCheckRule class >> uniqueIdentifierName [
	
	^ 'RBDefineBasickCheckRule'
]

{ #category : #running }
RBDefineBasickCheckRule >> basicCheck: aClass [ 
	
	^ (aClass inheritsFrom: RBLintRule) and: [ 
		aClass isVisible and: [ 
	  (aClass lookupSelector: #basicCheck:) isSubclassResponsibility ] ]
]

{ #category : #running }
RBDefineBasickCheckRule >> checkClass: aClass [ 
	(self basicCheck: aClass) ifTrue: [ result addClass: aClass ]
]

{ #category : #accessing }
RBDefineBasickCheckRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
RBDefineBasickCheckRule >> name [
	^ 'Rule does not define #basicCheck:'
]

{ #category : #accessing }
RBDefineBasickCheckRule >> resultClass [
	^ RBClassEnvironment
]
