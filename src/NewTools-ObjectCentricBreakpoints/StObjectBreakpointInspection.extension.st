Extension { #name : #StObjectBreakpointInspection }

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StObjectBreakpointInspection class >> browseBreakpointMethodCommandFor: presenter [

	| context cmd |
	context := [ :ctx | 
	           ctx methodsWithBreakpoints selection selectedItem
		           entityToBrowse ].
	cmd := SpBrowseClassCommand forSpecContext: presenter.
	cmd decoratedCommand
		transform: context;
		name: 'Browse method'.
	^ cmd
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StObjectBreakpointInspection class >> objectCentricCommandsWith: presenter forRootGroup: aCmCommandsGroup [
	<extensionCommands>
	aCmCommandsGroup
		register: (self removeBreakpointCommandFor: presenter).
	aCmCommandsGroup
		register: (self browseBreakpointMethodCommandFor: presenter)
]

{ #category : #'*NewTools-ObjectCentricBreakpoints' }
StObjectBreakpointInspection class >> removeBreakpointCommandFor: presenter [
	| cmd |
	cmd := StRemoveBreakpointCommand forSpecContext: presenter.
	cmd iconName: #smallDelete.
	cmd decoratedCommand
		transform: [ :ctx | ctx methodsWithBreakpoints selection selectedItem ].
	^ cmd
]
