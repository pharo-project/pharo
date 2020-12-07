Extension { #name : #Object }

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
Object >> inspectionBreakpoints [
	<inspectorPresentationOrder: 998 title: 'Breakpoints'>

	^ StObjectBreakpointInspection on: self
]

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
Object >> inspectionBreakpointsContext: aContext [

	aContext withoutEvaluator
]
