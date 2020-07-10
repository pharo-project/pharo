"
You are referencing a class that is obsolete, that is, it has been removed.

The class might have been added again, in this case the method needs to be recompiled.

"
Class {
	#name : #ReReferencesObsoleteClassRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #running }
ReReferencesObsoleteClassRule >> basicCheck: aNode [
	^ aNode isVariable 
		and: [aNode isGlobal
			and: [ aNode variable value isClassOrTrait
				and: [ aNode variable value isObsolete ] ] ]
]

{ #category : #helpers }
ReReferencesObsoleteClassRule >> critiqueFor: aNode [
	^ (super critiqueFor: aNode)
		tinyHint: aNode name;
		yourself
]

{ #category : #accessing }
ReReferencesObsoleteClassRule >> group [
	^ 'Bugs'
]

{ #category : #accessing }
ReReferencesObsoleteClassRule >> name [
	^ 'References an obsolete class'
]

{ #category : #accessing }
ReReferencesObsoleteClassRule >> severity [
	^ #error
]
