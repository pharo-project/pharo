"
This rule checks if all subclassResponsibility methods are defined in all leaf classes. if such a method is not overridden, a subclassResponsibility message can be occur when this method is called.
"
Class {
	#name : #RBSubclassResponsibilityNotDefinedRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
}

{ #category : #testing }
RBSubclassResponsibilityNotDefinedRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBSubclassResponsibilityNotDefinedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SubclassResponsibilityNotDefinedRule'
]

{ #category : #running }
RBSubclassResponsibilityNotDefinedRule >> basicCheck: aClass [ 
	^ (aClass whichSelectorsReferTo: #subclassResponsibility) anySatisfy:  
		[ :each | 
		 aClass subclasses 
			anySatisfy:  [ :class | 
				class subclasses isEmpty and: [ (class whichClassIncludesSelector: each) == aClass ] ] ]
]

{ #category : #running }
RBSubclassResponsibilityNotDefinedRule >> checkClass: aClass [ 
	(aClass whichSelectorsReferTo: #subclassResponsibility) do: 
		[ :each | 
		(aClass subclasses 
			anySatisfy:  
				[ :class | 
				class subclasses isEmpty and:
				[ (class whichClassIncludesSelector: each) == aClass ] ])  ifTrue: 
			[ result 
				addClass: aClass
				selector: each ] ]
]

{ #category : #accessing }
RBSubclassResponsibilityNotDefinedRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
RBSubclassResponsibilityNotDefinedRule >> name [
	^ 'Subclass responsibility not defined'
]

{ #category : #accessing }
RBSubclassResponsibilityNotDefinedRule >> rationale [
	^ 'Checks that all methods which send #subclassResponsibility, which indicates that they are abstract, are defined in all leaf classes.'
]

{ #category : #accessing }
RBSubclassResponsibilityNotDefinedRule >> severity [
	^ #error
]
