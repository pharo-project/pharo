"
Use uncapitalized instead of withFirstCharacterDownshifted since withFirstCharacterDownshifted is ugly and is deprecated now.
"
Class {
	#name : 'ReDeprecateWithFirstCharacterDownshiftedRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Style',
	#package : 'General-Rules',
	#tag : 'Style'
}

{ #category : 'accessing' }
ReDeprecateWithFirstCharacterDownshiftedRule class >> group [
	^ 'Style'
]

{ #category : 'accessing' }
ReDeprecateWithFirstCharacterDownshiftedRule class >> ruleName [
	^ 'Use uncapitalized instead of withFirstCharacterDownshifted'
]

{ #category : 'accessing' }
ReDeprecateWithFirstCharacterDownshiftedRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'FirstCharacterDownShifted'
]

{ #category : 'initialization' }
ReDeprecateWithFirstCharacterDownshiftedRule >> initialize [
	super initialize.
	self
		replace: '``@object withFirstCharacterDownshifted' with: '``@object uncapitalized'
]
