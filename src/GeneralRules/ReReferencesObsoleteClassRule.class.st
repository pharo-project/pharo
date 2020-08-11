"
You are referencing a class that is obsolete, that is, it has been removed

"
Class {
	#name : #ReReferencesObsoleteClassRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #helpers }
ReReferencesObsoleteClassRule >> basicCheck: aNode [
 	^ aNode isVariable 
 		and: [aNode isGlobalVariable
 			and: [ aNode variable isGlobalClassNameBinding
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
