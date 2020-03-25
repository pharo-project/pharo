"
Use uncapitalized instead of withFirstCharacterDownshifted since withFirstCharacterDownshifted is ugly and is deprecated now.
"
Class {
	#name : #ReDeprecateWithFirstCharacterDownshifted,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReDeprecateWithFirstCharacterDownshifted class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'FirstCharacterDownShifted'
]

{ #category : #accessing }
ReDeprecateWithFirstCharacterDownshifted >> group [
	^ 'Style'
]

{ #category : #initialization }
ReDeprecateWithFirstCharacterDownshifted >> initialize [
	super initialize.
	self
		replace: '``@object withFirstCharacterDownshifted' with: '``@object uncapitalized'.


]

{ #category : #accessing }
ReDeprecateWithFirstCharacterDownshifted >> name [
	^ 'use uncapitalized instead of withFirstCharacterDownshifted'
]
