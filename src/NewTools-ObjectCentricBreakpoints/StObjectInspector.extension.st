Extension { #name : #StObjectInspector }

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StObjectInspector >> haltInspectedObjectOnReadAccess [
	^self inspectedObject haltOnRead
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StObjectInspector >> haltInspectedObjectOnStateAccess [
	^ self inspectedObject haltOnAccess 
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StObjectInspector >> haltInspectedObjectOnWriteAccess [
	^ self inspectedObject haltOnWrite
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StObjectInspector class >> objectCentricBreakpointCommandsWith: presenter forRootGroup: aRootCmCommandsGroup [
	<extensionCommands>
	aRootCmCommandsGroup
		register: StHaltOnReadCommand forSpec;
		register: StHaltOnWriteCommand forSpec;
		register: StHaltOnStateAccessCommand forSpec
]
