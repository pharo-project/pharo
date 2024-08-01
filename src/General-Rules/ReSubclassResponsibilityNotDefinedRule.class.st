"
This rule checks if all subclassResponsibility methods are defined in all leaf classes. if such a method is not overridden, a subclassResponsibility message can be occur when this method is called.
"
Class {
	#name : 'ReSubclassResponsibilityNotDefinedRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'testing' }
ReSubclassResponsibilityNotDefinedRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReSubclassResponsibilityNotDefinedRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReSubclassResponsibilityNotDefinedRule class >> rationale [
	^ 'Checks that all methods which send #subclassResponsibility, which indicates that they are abstract, are defined in all leaf classes.'
]

{ #category : 'accessing' }
ReSubclassResponsibilityNotDefinedRule class >> ruleName [
	^ '#subclassResponsibility not defined'
]

{ #category : 'accessing' }
ReSubclassResponsibilityNotDefinedRule class >> severity [
	^ #error
]

{ #category : 'accessing' }
ReSubclassResponsibilityNotDefinedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'SubclassResponsibilityNotDefinedRule'
]

{ #category : 'running' }
ReSubclassResponsibilityNotDefinedRule >> check: aClass forCritiquesDo: aCriticBlock [

	(aClass whichMethodsReferTo: #subclassResponsibility) do:
		[ :method |
		(aClass subclasses do: [ :class |
				(class subclasses isEmpty and:
				[ (class whichClassIncludesSelector: method selector) == aClass ]) ifTrue: [
					aCriticBlock cull: (ReMissingMethodCritique
						for: aClass
						by: self
						class: class
						selector: method selector)
						beShouldBeImplemented;
						yourself ] ]) ]
]
