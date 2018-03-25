"
This smell arises when a class defines a method in all subclasses, but not in itself as an abstract method. Such methods should most likely be defined as subclassResponsibility methods. Furthermore, this check helps to find similar code that might be occurring in all the subclasses that should be pulled up into the superclass.
"
Class {
	#name : #RBMissingSubclassResponsibilityRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
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

{ #category : #enumerating }
RBMissingSubclassResponsibilityRule >> check: aClass forCritiquesDo: aCritiqueBlock [
	
	| subs |
	subs := aClass subclasses.
	(subs size > 1) ifTrue: 
		[ | sels noSuperSels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each selectors ].
		noSuperSels := sels asSet select: [ :each | 
			((sels occurrencesOf: each) == subs size and: [
			(aClass canUnderstand: each) not ]) ].
		noSuperSels ifNotEmpty: [ 
			noSuperSels collect: [ :selector |
				(ReMissingMethodCritique
					for: aClass
					by: self
					class: aClass
					selector: selector)
					beSubclassResponsibility;
					yourself ]
				thenDo: [ :critic |
					aCritiqueBlock cull: critic ].
				^ self ] ].
		
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
