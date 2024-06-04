"
You are referencing a class that is obsolete, that is, it has been removed

"
Class {
	#name : 'ReReferencesObsoleteClassRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Bugs',
	#package : 'General-Rules',
	#tag : 'Bugs'
}

{ #category : 'accessing' }
ReReferencesObsoleteClassRule class >> group [
	^ 'Bugs'
]

{ #category : 'accessing' }
ReReferencesObsoleteClassRule class >> ruleName [
	^ 'References an obsolete class'
]

{ #category : 'accessing' }
ReReferencesObsoleteClassRule class >> severity [
	^ #error
]

{ #category : 'helpers' }
ReReferencesObsoleteClassRule >> basicCheck: aNode [
 	^ aNode isGlobalVariable
 			and: [ aNode variable isGlobalClassNameBinding
 				and: [ aNode variable value isObsolete ] ]
]

{ #category : 'helpers' }
ReReferencesObsoleteClassRule >> critiqueFor: aNode [
	^ (super critiqueFor: aNode)
		  tinyHint: aNode name;
		  yourself
]
