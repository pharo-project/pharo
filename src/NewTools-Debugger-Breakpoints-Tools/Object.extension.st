Extension { #name : #Object }

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
Object >> breakpointInspection [
	<inspectorPresentationOrder: 998 title: 'Breakpoints'>

	^ StObjectBreakpointInspection on: self
]
