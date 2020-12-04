Extension { #name : #StRawInspection }

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StRawInspection >> haltInspectedObjectOnAccess: access [
	| selectedItem |
	selectedItem := self selectedItem.
	selectedItem ifNil:[
		"No selection: halt on all accesses"
		^ self model haltOnAccess: access ].
	selectedItem key asSymbol == #self 
		ifTrue:[
			"Self is selected: halt on all accesses"
			^ self model haltOnAccess: access].
	^ self model haltOnAccess: access to: selectedItem key
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StRawInspection >> haltInspectedObjectOnReadAccess [
	^self haltInspectedObjectOnAccess: #read
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StRawInspection >> haltInspectedObjectOnStateAccess [
	^ self haltInspectedObjectOnAccess: #all
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StRawInspection >> haltInspectedObjectOnWriteAccess [
	^ self haltInspectedObjectOnAccess: #write
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StRawInspection class >> objectCentricBreakpointCommandsWith: presenter forRootGroup: aRootCmCommandsGroup [
	<extensionCommands>
	({StHaltOnReadCommand forSpec.
	StHaltOnWriteCommand forSpec.
	StHaltOnStateAccessCommand forSpec}
		select: [ :spCommand | spCommand decoratedCommand appliesTo: presenter ])
		do: [ :spCommand | aRootCmCommandsGroup register: spCommand ]
]
