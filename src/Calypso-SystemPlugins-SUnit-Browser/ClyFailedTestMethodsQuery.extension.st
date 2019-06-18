Extension { #name : #ClyFailedTestMethodsQuery }

{ #category : #'*Calypso-SystemPlugins-SUnit-Browser' }
ClyFailedTestMethodsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	"we can detect that yellow color is needed but it looks bad"
	anItemCellMorph label color: Color red
	
]
