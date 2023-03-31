"
Provide rule to have no leading or trailing space in method category name
"
Class {
	#name : #ReNoSpaceAroundProtocolRule,
	#superclass : #ReAbstractRule,
	#instVars : [
		'protocolName'
	],
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReNoSpaceAroundProtocolRule class >> checksMethod [

	^ true
]

{ #category : #manifest }
ReNoSpaceAroundProtocolRule class >> uniqueIdentifierName [

	^ 'ReNoSpaceAroundMethodCategoryNameRule'
]

{ #category : #running }
ReNoSpaceAroundProtocolRule >> basicCheck: aMethod [

	protocolName := aMethod protocol.
	^ (protocolName endsWith: ' ') or: [ protocolName beginsWith: ' ' ]
]

{ #category : #running }
ReNoSpaceAroundProtocolRule >> critiqueFor: aMethod [

	| proposedProtocol |
	proposedProtocol := protocolName trimBoth.

	^ (ReRefactoringCritique withAnchor: (self anchorFor: aMethod) by: self) refactoring:
		  (RBMethodProtocolTransformation protocol: { proposedProtocol } inMethod: aMethod selector inClass: aMethod methodClass name asSymbol) asRefactoring
]

{ #category : #accessing }
ReNoSpaceAroundProtocolRule >> group [

	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
ReNoSpaceAroundProtocolRule >> name [

	^ 'Protocol named "{1}" should be trimmed (includes space at start or end)' format: { protocolName }
]
