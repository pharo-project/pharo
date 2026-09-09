Extension { #name : 'EFFormatter' }

{ #category : '*Deprecated14' }
EFFormatter class >> periodsAtEndOfBlock [

	self deprecated: 'Use #periodAtEndOfBlock instead.' transformWith: '`@rcv periodsAtEndOfBlock' -> '`@rcv periodAtEndOfBlock'.
	^ self periodAtEndOfBlock
]

{ #category : '*Deprecated14' }
EFFormatter >> periodsAtEndOfBlock [

	self deprecated: 'Use #periodAtEndOfBlock instead.' transformWith: '`@rcv periodsAtEndOfBlock' -> '`@rcv periodAtEndOfBlock'.
	^ self periodAtEndOfBlock
]

{ #category : '*Deprecated14' }
EFFormatter class >> periodsAtEndOfMethod [

	self deprecated: 'Use #periodAtEndOfMethod instead.' transformWith: '`@rcv periodsAtEndOfMethod' -> '`@rcv periodAtEndOfMethod'.
	^ self periodAtEndOfMethod
]

{ #category : '*Deprecated14' }
EFFormatter >> periodsAtEndOfMethod [

	self deprecated: 'Use #periodAtEndOfMethod instead.' transformWith: '`@rcv periodsAtEndOfMethod' -> '`@rcv periodAtEndOfMethod'.
	^ self periodAtEndOfMethod
]

{ #category : '*Deprecated14' }
EFFormatter class >> settingsPeriodsAtEndOfBlock: aBuilder [

	self
		deprecated: 'Use #settingsPeriodAtEndOfBlock: instead'
		transformWith: '`@rcv settingsPeriodsAtEndOfBlock: `@aBuilder' -> '`@rcv settingsPeriodAtEndOfBlock: `@aBuilder'.
	^ self settingsPeriodAtEndOfBlock: aBuilder
]

{ #category : '*Deprecated14' }
EFFormatter class >> settingsPeriodsAtEndOfMethod: aBuilder [

	self
		deprecated: 'Use #settingsPeriodAtEndOfMethod: instead'
		transformWith: '`@rcv settingsPeriodsAtEndOfMethod: `@aBuilder' -> '`@rcv settingsPeriodAtEndOfMethod: `@aBuilder'.
	^ self settingsPeriodAtEndOfMethod: aBuilder
]
