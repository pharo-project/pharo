Extension { #name : #ClyAllProblemMethodQuery }

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyAllProblemMethodQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph definitionIcon: #smallDebug
]
