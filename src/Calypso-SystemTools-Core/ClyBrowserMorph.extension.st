Extension { #name : 'ClyBrowserMorph' }

{ #category : '*Calypso-SystemTools-Core' }
ClyBrowserMorph >> allowSilentlyRemoveMethods: methods whichHaveSenders: senders [

	| singleSender sendersCount |
	sendersCount := senders size.
	sendersCount = 0 ifTrue: [ ^true].
	methods size = 1 & (sendersCount = 1) ifFalse: [^false].

	singleSender := senders itemAt: 1.
	^singleSender == methods first
]

{ #category : '*Calypso-SystemTools-Core' }
ClyBrowserMorph >> confirmEmptySystemQuery: aQuery [

	| showResult result |

	result := aQuery execute.
	result isEmpty ifTrue: [ ^true ].

	showResult := self morphicUIManager confirm: 'There are '
		, result size asString, ' ', aQuery description, '. Show them?'.
	showResult ifTrue: [self spawnQueryBrowserOn: aQuery].
	^false
]

{ #category : '*Calypso-SystemTools-Core' }
ClyBrowserMorph >> confirmUnusedVariablesInDefiningClass: variables [

	| refQuery classScope |
	classScope := ClyBothMetaLevelClassScope ofAll: (variables collect: [:each | each definingClass]) in: navigationEnvironment.
	refQuery := ClyVariableReferencesQuery ofAny: variables from: classScope.

	^self confirmEmptySystemQuery: refQuery
]

{ #category : '*Calypso-SystemTools-Core' }
ClyBrowserMorph >> decorateMethodEditor: aMethodEditor [
]

{ #category : '*Calypso-SystemTools-Core' }
ClyBrowserMorph >> isMethodSelected: aMethod [

	^self methodSelection includesActualObject: aMethod
]
