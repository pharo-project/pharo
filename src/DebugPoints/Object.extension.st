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
		  On: (self class compiledMethodAt: aMethodSelector) ast
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
		  On: (self class compiledMethodAt: aMethodSelector) ast
		  forObject: self
		  withBehaviors: { OnceBehavior }
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
