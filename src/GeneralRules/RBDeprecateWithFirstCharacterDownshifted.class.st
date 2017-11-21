"
Use uncapitalized instead of withFirstCharacterDownshifted since withFirstCharacterDownshifted is ugly and is deprecated now.
"
Class {
	#name : #RBDeprecateWithFirstCharacterDownshifted,
	#superclass : #ReNodeRewriteRule,
	#category : #GeneralRules-Migrated
}

{ #category : #accessing }
RBDeprecateWithFirstCharacterDownshifted class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'FirstCharacterDownShifted'
]

{ #category : #accessing }
RBDeprecateWithFirstCharacterDownshifted >> group [
	^ 'Style'
]

{ #category : #initialization }
RBDeprecateWithFirstCharacterDownshifted >> initialize [
	super initialize.
	self
		replace: '``@object withFirstCharacterDownshifted' with: '``@object uncapitalized'.


]

{ #category : #accessing }
RBDeprecateWithFirstCharacterDownshifted >> name [
	^ 'use uncapitalized instead of withFirstCharacterDownshifted'
]
