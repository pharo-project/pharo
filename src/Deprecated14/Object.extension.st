Extension { #name : 'Object' }

{ #category : '*Deprecated14' }
Object >> haltOnAccess [

	self
		deprecated: 'Use #breakOnReadWrite instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnAccess' -> '`@receiver breakOnReadWrite'.

	^ self breakOnReadWrite
]

{ #category : '*Deprecated14' }
Object >> haltOnAccess: access [

	self
		deprecated: 'Use #breakOnAccess: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnAccess: `@arg' -> '`@receiver breakOnAccess: `@arg'.

	^ self breakOnAccess: access
]

{ #category : '*Deprecated14' }
Object >> haltOnAccess: access to: anInstVarName [

	self
		deprecated: 'Use #breakOnAccess:toSlotNamed: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnAccess: `@arg1 to: `@arg2' -> '`@receiver breakOnAccess: `@arg toSlotNamed: `@arg2'.

	^ self breakOnAccess: access toSlotNamed: anInstVarName
]

{ #category : '*Deprecated14' }
Object >> haltOnAccessTo: aVariableName [

	self
		deprecated: 'Use #breakOnReadWriteTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnAccessTo: `@arg' -> '`@receiver breakOnReadWriteTo: `@arg'.

	^ self breakOnReadWriteTo: aVariableName
]

{ #category : '*Deprecated14' }
Object >> haltOnCallTo: aSelector [

	self
		deprecated: 'Use #breakOnCallTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnCallTo: `@arg' -> '`@receiver breakOnCallTo: `@arg'.

	^ self breakOnCallTo: aSelector
]

{ #category : '*Deprecated14' }
Object >> haltOnRead [

	self
		deprecated: 'Use #breakOnRead instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnRead' -> '`@receiver breakOnRead'.

	^ self breakOnRead
]

{ #category : '*Deprecated14' }
Object >> haltOnReadTo: aVariableName [

	self
		deprecated: 'Use #breakOnReadTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnReadTo: `@arg' -> '`@receiver breakOnReadTo: `@arg'.
	
	^ self breakOnReadTo: aVariableName
]

{ #category : '*Deprecated14' }
Object >> haltOnWrite [

	self
		deprecated: 'Use #breakOnWrite instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnWrite' -> '`@receiver breakOnWrite'.

	^ self breakOnWrite
]

{ #category : '*Deprecated14' }
Object >> haltOnWriteTo: aVariableName [

	self
		deprecated: 'Use #breakOnWriteTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnWriteTo: `@arg' -> '`@receiver breakOnWriteTo: `@arg'.
		
	^ self breakOnWriteTo: aVariableName
]

{ #category : '*Deprecated14' }
Object >> haltOnceOnCallTo: aSelector [

	self
		deprecated: 'Use #breakOnceOnCallTo: instead'
		on: '28/05/2025'
		in: #Pharo14
		transformWith: '`@receiver haltOnceOnCallTo: `@arg' -> '`@receiver breakOnceOnCallTo: `@arg'.

	^ self breakOnceOnCallTo: aSelector
]

{ #category : '*Deprecated14' }
Object >> taskbarIcon [
	"Answer the icon for the receiver in a task bar
	or nil for the default."

	self
		deprecated: 'The direct usage of #taskbarIcon is not recommended anymore since it
	introduces a dependency with UI layer we do not want to have here. Instead, our UI
	components should us #taskbarIconName to get the icon to use, and then user can resolve it
	as their want (since different applications can have different iconpacks, etc.) #Pharo11'
		transformWith: '`@receiver taskbarIcon' -> '`@receiver iconNamed: `@receiver taskbarIconName'.

	^ self iconNamed: self taskbarIconName
]
