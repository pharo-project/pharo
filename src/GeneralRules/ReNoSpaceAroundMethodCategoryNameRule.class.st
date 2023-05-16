"
Provide rule to have no leading or trailing space in method category name
"
Class {
	#name : #ReNoSpaceAroundMethodCategoryNameRule,
	#superclass : #ReAbstractRule,
	#instVars : [
		#protocolName
	],
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReNoSpaceAroundMethodCategoryNameRule class >> checksMethod [

	^ true
]

{ #category : #manifest }
ReNoSpaceAroundMethodCategoryNameRule class >> uniqueIdentifierName [

	^ 'ReNoSpaceAroundMethodCategoryNameRule'
]

{ #category : #running }
ReNoSpaceAroundMethodCategoryNameRule >> basicCheck: aMethod [
	protocolName := aMethod protocol.
	^ (protocolName endsWith: ' ')
			or: [ protocolName beginsWith: ' ' ]
]

{ #category : #running }
ReNoSpaceAroundMethodCategoryNameRule >> critiqueFor: aMethod [
	| proposedCategory |
	proposedCategory := protocolName trimBoth.

	^ (ReRefactoringCritique
		   withAnchor: (self anchorFor: aMethod)
		   by: self) refactoring: (RBMethodProtocolTransformation
			   protocol: { proposedCategory }
			   inMethod: aMethod selector
			   inClass: aMethod methodClass name asSymbol) asRefactoring
]

{ #category : #accessing }
ReNoSpaceAroundMethodCategoryNameRule >> group [

	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
ReNoSpaceAroundMethodCategoryNameRule >> name [

	^ 'Category name "{1}" should be trimmed (includes space at start or end)'
		format: { protocolName }
]
