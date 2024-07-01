"
This smell arises when a deep inheritance is found (depth of ten or more), which is usually a sign of a design flaw. It should be broken down and reduced to something manageable. The defined inheritance depth can be edited in RBExcessiveInheritanceRule>>inheritanceDepth.

Several possibilities can occur. Here are some hints:
- check whether some classes in the hierarchy just do not add enough behavior to require a class in itself
- check whether all the classes are the root of a kind of little inheritance hierarchy. 

Note that often a framework may already define a certain level of inheritance, with  other layers added by user code. This rule does not take these frameworks into account. 	
	
The defined inheritance depth can be edited in #inheritanceDepth.
"
Class {
	#name : 'ReExcessiveInheritanceRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing' }
ReExcessiveInheritanceRule class >> checksClass [
	^ true
]

{ #category : 'accessing' }
ReExcessiveInheritanceRule class >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReExcessiveInheritanceRule class >> rationale [
	^ 'Deep inheritance (10+ depth) is usually a sign of a design flaw.'
]

{ #category : 'accessing' }
ReExcessiveInheritanceRule class >> ruleName [
	^ 'Excessive inheritance depth'
]

{ #category : 'accessing' }
ReExcessiveInheritanceRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'ExcessiveInheritanceRule'
]

{ #category : 'enumerating' }
ReExcessiveInheritanceRule >> basicCheck: aClass [
	aClass isMeta ifTrue: [ ^ false ].

	^ aClass allSuperclasses size >= self inheritanceDepth
]

{ #category : 'private' }
ReExcessiveInheritanceRule >> inheritanceDepth [
	^ 10
]
