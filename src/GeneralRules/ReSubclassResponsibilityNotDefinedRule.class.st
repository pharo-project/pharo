"
This rule checks if all subclassResponsibility methods are defined in all leaf classes. if such a method is not overridden, a subclassResponsibility message can be occur when this method is called.
"
Class {
	#name : #ReSubclassResponsibilityNotDefinedRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReSubclassResponsibilityNotDefinedRule class >> checksClass [
	^ true
]

{ #category : #accessing }
ReSubclassResponsibilityNotDefinedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SubclassResponsibilityNotDefinedRule'
]

{ #category : #running }
ReSubclassResponsibilityNotDefinedRule >> check: aClass forCritiquesDo: aCriticBlock [

	(aClass whichSelectorsReferTo: #subclassResponsibility) do:  
		[ :selector | 
		(aClass subclasses do: [ :class | 
				(class subclasses isEmpty and:
				[ (class whichClassIncludesSelector: selector) == aClass ]) ifTrue: [ 
					aCriticBlock cull: (ReMissingMethodCritique
						for: aClass
						by: self
						class: class
						selector: selector)
						beShouldBeImplemented;
						yourself ] ]) ].
	
]

{ #category : #accessing }
ReSubclassResponsibilityNotDefinedRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReSubclassResponsibilityNotDefinedRule >> name [
	^ '#subclassResponsibility not defined'
]

{ #category : #accessing }
ReSubclassResponsibilityNotDefinedRule >> rationale [
	^ 'Checks that all methods which send #subclassResponsibility, which indicates that they are abstract, are defined in all leaf classes.'
]

{ #category : #accessing }
ReSubclassResponsibilityNotDefinedRule >> severity [
	^ #error
]
