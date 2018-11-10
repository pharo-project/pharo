Extension { #name : #ClyActiveBreakpoints }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyActiveBreakpoints >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #smallDebug
]

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyActiveBreakpoints >> decorateResultMethodEditor: aMethodEditor [

	aMethodEditor selectAnySelector: #(halt halt: haltIf: haltOnce haltOnCount)
]
