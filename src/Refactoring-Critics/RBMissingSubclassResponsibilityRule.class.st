"
This smell arises when a class defines a method in all subclasses, but not in itself as an abstract method. Such methods should most likely be defined as subclassResponsibility methods. Furthermore, this check helps to find similar code that might be occurring in all the subclasses that should be pulled up into the superclass.
"
Class {
	#name : #RBMissingSubclassResponsibilityRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBMissingSubclassResponsibilityRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBMissingSubclassResponsibilityRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'MissingSubclassResponsibilityRule'
]

{ #category : #running }
RBMissingSubclassResponsibilityRule >> basicCheck: aClass [ 
	| subs |
	subs := aClass subclasses.
	(subs size > 1 and: [ aClass isMeta not ]) ifTrue: 
		[ | sels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each selectors ].
		^ sels asSet anySatisfy: 
			[ :each |
			((sels occurrencesOf: each) == subs size and: [ (aClass canUnderstand: each) not ]) ] ].
	
		^ false.
]

{ #category : #running }
RBMissingSubclassResponsibilityRule >> checkClass: aClass [ 
	| subs |
	subs := aClass subclasses.
	(subs size > 1 and: [ aClass isMeta not ]) ifTrue: 
		[ | sels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each selectors ].
		sels asSet do: 
			[ :each | 
			((sels occurrencesOf: each) == subs size and: [ (aClass canUnderstand: each) not ]) ifTrue: 
				[ | envName |
				envName := aClass name , '>>' , each.
				subs do: 
					[ :subClass | 
					result 
						addClass: subClass
						selector: each
						into: envName ] ] ] ]
]

{ #category : #accessing }
RBMissingSubclassResponsibilityRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBMissingSubclassResponsibilityRule >> name [
	^ 'Method defined in all subclasses, but not in superclass'
]

{ #category : #accessing }
RBMissingSubclassResponsibilityRule >> resultClass [
	^ RBMultiEnvironment
]
