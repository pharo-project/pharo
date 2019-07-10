Extension { #name : #ClyActiveBreakpointsQuery }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyActiveBreakpointsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #smallDebug
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyActiveBreakpointsQuery >> decorateResultMethodEditor: aMethodEditor [

	aMethodEditor selectAnySelector: #(halt halt: haltIf: haltOnce haltOnCount)
]
