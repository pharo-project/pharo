"
Sometimes instance variables are used instead of method arguments or temporaries. You can check if an instance variable's value is always valid during the complete lifetime of an object. In addition, looking at methods used together by a group of methods may be an indication that a new object should be created. Large classes often exhibit a large number of instance variables. Some people also confuse classes and namespaces. 

The defined number of instance variables can be edited in #variablesCount.
"
Class {
	#name : #RBExcessiveVariablesRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBExcessiveVariablesRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBExcessiveVariablesRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ExcessiveVariablesRule'
]

{ #category : #running }
RBExcessiveVariablesRule >> basicCheck: aClass [
	^ aClass instVarNames size >= self variablesCount or: [ aClass classVarNames size >= self variablesCount ]
]

{ #category : #accessing }
RBExcessiveVariablesRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBExcessiveVariablesRule >> name [
	^ 'Excessive number of variables'
]

{ #category : #accessing }
RBExcessiveVariablesRule >> rationale [
	^ 'Classes that have too many instance variables (10+) could be redesigned to have fewer fields, possibly through some nested object grouping.'
]

{ #category : #private }
RBExcessiveVariablesRule >> variablesCount [
	^ 10
]
