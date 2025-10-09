Extension { #name : 'Object' }

{ #category : '*DebugPoints' }
Object >> breakOnAccess: anAccessStrategySymbol [

	^ self class allSlots do: [ :slot |
		  self breakOnAccess: anAccessStrategySymbol toSlotNamed: slot name ]
]

{ #category : '*DebugPoints' }
Object >> breakOnAccess: anAccessStrategySymbol toSlotNamed: slotName [

	^ self
		  installDebugPoint: BreakDebugPoint
		  onSlotNamed: slotName
		  onAccess: anAccessStrategySymbol
]

{ #category : '*DebugPoints' }
Object >> breakOnCallTo: aMethodSelector [

	^ DebugPointManager
		  installNew: BreakDebugPoint
		  on: (self class compiledMethodAt: aMethodSelector) ast
		  forObject: self
]

{ #category : '*DebugPoints' }
Object >> breakOnRead [

	^ self breakOnAccess: #read
]

{ #category : '*DebugPoints' }
Object >> breakOnReadTo: aSlotName [

	^ self breakOnAccess: #read toSlotNamed: aSlotName
]

{ #category : '*DebugPoints' }
Object >> breakOnReadWrite [

	^ self breakOnAccess: #all
]

{ #category : '*DebugPoints' }
Object >> breakOnReadWriteTo: aSlotName [

	^ self breakOnAccess: #all toSlotNamed: aSlotName
]

{ #category : '*DebugPoints' }
Object >> breakOnWrite [

	^ self breakOnAccess: #write
]

{ #category : '*DebugPoints' }
Object >> breakOnWriteTo: aSlotName [

	^ self breakOnAccess: #write toSlotNamed: aSlotName
]

{ #category : '*DebugPoints' }
Object >> breakOnceOnCallTo: aMethodSelector [

	^ DebugPointManager
		  installNew: BreakDebugPoint
		  on: (self class compiledMethodAt: aMethodSelector) ast
		  forObject: self
		  withBehaviors: { OnceBehavior }
]

{ #category : '*DebugPoints' }
Object >> haltOnAccess [

	self
		deprecated: 'Use #breakOnReadWrite instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnAccess' -> '`@receiver breakOnReadWrite'.

	^ self breakOnReadWrite
]

{ #category : '*DebugPoints' }
Object >> haltOnAccess: access [

	self
		deprecated: 'Use #breakOnAccess: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnAccess: `@arg' -> '`@receiver breakOnAccess: `@arg'.

	^ self breakOnAccess: access
]

{ #category : '*DebugPoints' }
Object >> haltOnAccess: access to: anInstVarName [

	self
		deprecated: 'Use #breakOnAccess:toSlotNamed: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnAccess: `@arg1 to: `@arg2' -> '`@receiver breakOnAccess: `@arg toSlotNamed: `@arg2'.

	^ self breakOnAccess: access toSlotNamed: anInstVarName
]

{ #category : '*DebugPoints' }
Object >> haltOnAccessTo: aVariableName [

	self
		deprecated: 'Use #breakOnReadWriteTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnAccessTo: `@arg' -> '`@receiver breakOnReadWriteTo: `@arg'.

	^ self breakOnReadWriteTo: aVariableName
]

{ #category : '*DebugPoints' }
Object >> haltOnCallTo: aSelector [

	self
		deprecated: 'Use #breakOnCallTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnCallTo: `@arg' -> '`@receiver breakOnCallTo: `@arg'.

	^ self breakOnCallTo: aSelector
]

{ #category : '*DebugPoints' }
Object >> haltOnRead [

	self
		deprecated: 'Use #breakOnRead instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnRead' -> '`@receiver breakOnRead'.

	^ self breakOnRead
]

{ #category : '*DebugPoints' }
Object >> haltOnReadTo: aVariableName [

	self
		deprecated: 'Use #breakOnReadTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnReadTo: `@arg' -> '`@receiver breakOnReadTo: `@arg'.
	
	^ self breakOnReadTo: aVariableName
]

{ #category : '*DebugPoints' }
Object >> haltOnWrite [

	self
		deprecated: 'Use #breakOnWrite instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnWrite' -> '`@receiver breakOnWrite'.

	^ self breakOnWrite
]

{ #category : '*DebugPoints' }
Object >> haltOnWriteTo: aVariableName [

	self
		deprecated: 'Use #breakOnWriteTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnWriteTo: `@arg' -> '`@receiver breakOnWriteTo: `@arg'.
		
	^ self breakOnWriteTo: aVariableName
]

{ #category : '*DebugPoints' }
Object >> haltOnceOnCallTo: aSelector [

	self
		deprecated: 'Use #breakOnceOnCallTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnceOnCallTo: `@arg' -> '`@receiver breakOnceOnCallTo: `@arg'.

	^ self breakOnceOnCallTo: aSelector
]

{ #category : '*DebugPoints' }
Object >> installDebugPoint: aDebugPointClass onSlotNamed: slotName onAccess: anAccessStrategySymbol [

	^ DebugPointManager
		  installNew: aDebugPointClass
		  forObject: self
		  onVariableNamed: slotName
		  accessStrategy: anAccessStrategySymbol
]

{ #category : '*DebugPoints' }
Object >> watchOnAccess: anAccessStrategySymbol [

	^ self class allSlots do: [ :slot |
		  self watchOnAccess: anAccessStrategySymbol toSlotNamed: slot name ]
]

{ #category : '*DebugPoints' }
Object >> watchOnAccess: anAccessStrategySymbol toSlotNamed: slotName [

	^ self
		  installDebugPoint: WatchDebugPoint
		  onSlotNamed: slotName
		  onAccess: anAccessStrategySymbol
]

{ #category : '*DebugPoints' }
Object >> watchOnRead [

	^ self watchOnAccess: #read
]

{ #category : '*DebugPoints' }
Object >> watchOnReadTo: aSlotName [

	^ self watchOnAccess: #read toSlotNamed: aSlotName
]

{ #category : '*DebugPoints' }
Object >> watchOnReadWrite [

	^ self watchOnAccess: #all
]

{ #category : '*DebugPoints' }
Object >> watchOnReadWriteTo: aSlotName [

	^ self watchOnAccess: #all toSlotNamed: aSlotName
]

{ #category : '*DebugPoints' }
Object >> watchOnWrite [

	^ self watchOnAccess: #write
]

{ #category : '*DebugPoints' }
Object >> watchOnWriteTo: aSlotName [

	^ self watchOnAccess: #write toSlotNamed: aSlotName
]
