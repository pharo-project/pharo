Extension { #name : #StMetaBrowser }

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StMetaBrowser >> haltInspectedObjectOnCallToSelectedMethod [
	|method|
	method := self selectedMethod ifNil:[^self].
	self model haltOnCallTo: method selector
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StMetaBrowser class >> objectCentricBreakpointCommandsWith: presenter forRootGroup: aRootCmCommandsGroup [
	<extensionCommands>
	aRootCmCommandsGroup
		register: StHaltOnCallCommand forSpec
]
