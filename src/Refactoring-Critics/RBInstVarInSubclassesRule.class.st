"
This smell arises when instance variables are defined in all subclasses. Many times you might want to pull the instance variable up into the class so that all the subclasses do not have to define it. In addition have a look at the initialize method in each of the subclasses because if the instance variable is really the same, it will be initialized similarly in different places.


"
Class {
	#name : #RBInstVarInSubclassesRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
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
RBInstVarInSubclassesRule >> basicCheck: aClass [ 
	| subs |
	subs := aClass subclasses.
	subs size > 1 ifTrue: 
		[ | sels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each instVarNames ].
		^ sels asSet anySatisfy:  
			[ :val | 
			| count |
			count := sels occurrencesOf: val.
			count == subs size ] ].
	
	^ false
]

{ #category : #running }
RBInstVarInSubclassesRule >> checkClass: aClass [ 
	| subs |
	subs := aClass subclasses.
	subs size > 1 ifTrue: 
		[ | sels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each instVarNames ].
		sels asSet do: 
			[ :val | 
			| count |
			count := sels occurrencesOf: val.
			count == subs size ifTrue: 
				[ result 
					addClass: aClass
					instanceVariable: val ] ] ]
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
RBInstVarInSubclassesRule >> resultClass [
	^ RBVariableEnvironment
]

{ #category : #accessing }
RBInstVarInSubclassesRule >> severity [
	^ #information
]
