"
This smell arises when a deep inheritance is found (depth of ten or more), which is usually a sign of a design flaw. It should be broken down and reduced to something manageable. The defined inheritance depth can be edited in RBExcessiveInheritanceRule>>inheritanceDepth.

Several possibilities can occur. Here are some hints:
- check whether some classes in the hierarchy just do not add enough behavior to require a class in itself
- check whether all the classes are the root of a kind of little inheritance hierarchy. 

Note that often a framework may already define a certain level of inheritance, with  other layers added by user code. This rule does not take these frameworks into account. 	
	
The defined inheritance depth can be edited in #inheritanceDepth.
"
Class {
	#name : #RBExcessiveInheritanceRule,
	#superclass : #RBBlockLintRule,
	#category : #Refactoring-Critics-BlockRules
}

{ #category : #testing }
RBExcessiveInheritanceRule class >> checksClass [
	^ true
]

{ #category : #accessing }
RBExcessiveInheritanceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'ExcessiveInheritanceRule'
]

{ #category : #running }
RBExcessiveInheritanceRule >> basicCheck: aClass [
	| count current |
	aClass isMeta ifTrue: [ ^ false ].
	count := 1.
	current := aClass.
	[ current isNil ] whileFalse: [
		self inheritanceDepth < count
			ifTrue: [ ^ true ].
		current := current superclass.
		count := count + 1 ].
	
	^ false
]

{ #category : #running }
RBExcessiveInheritanceRule >> checkClass: aClass [
	(self basicCheck: aClass)
		ifTrue: [ result addClassAndMetaClassFor: aClass ]
]

{ #category : #accessing }
RBExcessiveInheritanceRule >> group [
	^ 'Design Flaws'
]

{ #category : #private }
RBExcessiveInheritanceRule >> inheritanceDepth [
	^ 10
]

{ #category : #accessing }
RBExcessiveInheritanceRule >> name [
	^ 'Excessive inheritance depth'
]

{ #category : #accessing }
RBExcessiveInheritanceRule >> rationale [
	^ 'Deep inheritance (10+ depth) is usually a sign of a design flaw.'
]

{ #category : #accessing }
RBExcessiveInheritanceRule >> resultClass [
	^ RBClassEnvironment
]
