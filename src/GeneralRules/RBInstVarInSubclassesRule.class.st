"
This smell arises when instance variables are defined in all subclasses. Many times you might want to pull the instance variable up into the class so that all the subclasses do not have to define it. In addition have a look at the initialize method in each of the subclasses because if the instance variable is really the same, it will be initialized similarly in different places.


"
Class {
	#name : #RBInstVarInSubclassesRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
RBInstVarInSubclassesRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBInstVarInSubclassesRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'InstVarInSubclassesRule'
]

{ #category : #running }
RBInstVarInSubclassesRule >> check: aClass forCritiquesDo: aCritiqueBlock [
 
	| subs sels |
	subs := aClass subclasses.
	subs size > 1 ifFalse: [ ^ self ].

	sels := subs flatCollect: #instVarNames as: Bag.
	
	sels asSet
		select: [ :sel | 
			(sels occurrencesOf: sel) == subs size ]
		thenDo: [ :sel | aCritiqueBlock cull: (
			(self critiqueFor: aClass)
				tinyHint: sel;
				refactoring: (
					RBPullUpInstanceVariableRefactoring
						variable: sel
						class: aClass);
				yourself) ]
]

{ #category : #running }
RBInstVarInSubclassesRule >> critiqueFor: aClass [
	^ ReRefactoringCritique
			withAnchor: (self anchorFor: aClass)
			by: self
]

{ #category : #accessing }
RBInstVarInSubclassesRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
RBInstVarInSubclassesRule >> name [
	^ 'Same instance variable defined in ALL subclasses'
]

{ #category : #accessing }
RBInstVarInSubclassesRule >> rationale [
	^ 'All subclasses of this class define the same variable. Most likely this variable should be pulled up to the superclass.'
]

{ #category : #accessing }
RBInstVarInSubclassesRule >> severity [
	^ #information
]
