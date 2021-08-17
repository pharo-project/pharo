"
Use uncapitalized instead of withFirstCharacterDownshifted since withFirstCharacterDownshifted is ugly and is deprecated now.
"
Class {
	#name : #ReDeprecateWithFirstCharacterDownshiftedRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReDeprecateWithFirstCharacterDownshiftedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'FirstCharacterDownShifted'
]

{ #category : #accessing }
ReDeprecateWithFirstCharacterDownshiftedRule >> group [
	^ 'Style'
]

{ #category : #initialization }
ReDeprecateWithFirstCharacterDownshiftedRule >> initialize [
	super initialize.
	self
		replace: '``@object withFirstCharacterDownshifted' with: '``@object uncapitalized'.


]

{ #category : #accessing }
ReDeprecateWithFirstCharacterDownshiftedRule >> name [
	^ 'Use uncapitalized instead of withFirstCharacterDownshifted'
]
