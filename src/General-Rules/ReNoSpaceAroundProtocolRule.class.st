"
Provide rule to have no leading or trailing space in method category name
"
Class {
	#name : 'ReNoSpaceAroundProtocolRule',
	#superclass : 'ReAbstractRule',
	#instVars : [
		'protocolName'
	],
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'testing' }
ReNoSpaceAroundProtocolRule class >> checksMethod [

	^ true
]

{ #category : 'accessing' }
ReNoSpaceAroundProtocolRule class >> group [

	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReNoSpaceAroundProtocolRule class >> ruleName [

	^ 'Protocol should be trimmed (includes space at start or end)'
]

{ #category : 'manifest' }
ReNoSpaceAroundProtocolRule class >> uniqueIdentifierName [

	^ 'ReNoSpaceAroundMethodCategoryNameRule'
]

{ #category : 'running' }
ReNoSpaceAroundProtocolRule >> basicCheck: aMethod [

	protocolName := aMethod protocolName.
	^ (protocolName endsWith: ' ') or: [ protocolName beginsWith: ' ' ]
]

{ #category : 'running' }
ReNoSpaceAroundProtocolRule >> critiqueFor: aMethod [

	| proposedProtocol |
	proposedProtocol := protocolName trimBoth.

	^ (ReRefactoringCritique
		   withAnchor: (self anchorFor: aMethod)
		   by: self) refactoring: (RBMethodProtocolTransformation
			   protocol: { proposedProtocol }
			   inMethod: aMethod selector
			   inClass: aMethod methodClass name asSymbol)
]
