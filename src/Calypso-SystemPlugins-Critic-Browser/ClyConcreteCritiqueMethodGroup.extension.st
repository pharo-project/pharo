Extension { #name : #ClyConcreteCritiqueMethodGroup }

{ #category : #'*Calypso-SystemPlugins-Critic-Browser' }
ClyConcreteCritiqueMethodGroup >> decorateTableCell: anItemCellMorph of: groupItem [
		
	anItemCellMorph definitionIcon: ruleSeverity iconName
]
