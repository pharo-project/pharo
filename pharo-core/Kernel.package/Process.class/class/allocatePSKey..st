process specific
allocatePSKey: aPSVariable

	"Add a new process-specific key. 
	If an object already registered as a key, answer its index,
	if object is not registered, first search for an empty slot for insertion and if not found, grow an array to add new object"

	| index |
	PSKeysSema ifNil: [ PSKeysSema := Semaphore forMutualExclusion ].
	PSKeysSema critical: [
		PSKeys 
			ifNil: [ PSKeys := WeakArray with: aPSVariable. index := 1 ]
			ifNotNil: [ 
				index := PSKeys indexOf: aPSVariable.
				index = 0 ifTrue: [
					index := PSKeys indexOf: nil.
					index = 0 
						ifTrue: [
							index := (PSKeys := PSKeys copyWith: aPSVariable) size ]
						ifFalse: [
							"Yes, this is slow, but we have to make sure that if we reusing index,
							all existing processes having value at given index reset to nil.
							We don't care if new processes will be created during this loop,
							since their env variable will be initially nil anyways, hence nothing to reset "
							Process allSubInstancesDo: [:p | p resetPSValueAt: index ].
							PSKeys at: index put: aPSVariable.
							]
				]
			].

		aPSVariable isInheritable ifTrue: [ 
			InheritablePSKeys 
				ifNil: [ InheritablePSKeys := Array with: index ]
				ifNotNil: [ 
					(InheritablePSKeys includes: index) ifFalse: [ InheritablePSKeys := InheritablePSKeys copyWith: index ]]
		]
	].

	^ index