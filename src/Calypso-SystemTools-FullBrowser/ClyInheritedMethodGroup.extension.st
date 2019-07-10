Extension { #name : #ClyInheritedMethodGroup }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyInheritedMethodGroup >> decorateTableCell: anItemCellMorph of: groupItem [
	super decorateTableCell: anItemCellMorph of: groupItem.
	anItemCellMorph setBalloonText: self tooltipText.
	
	visibilityLevels do: [ :each | 
		each decorateMethodGroupTableCell: anItemCellMorph of: groupItem ]
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyInheritedMethodGroup >> tooltipText [

	^'Expand me to toggle method visibility of concrete class.
Use checkbox at the right of label to make all inherited methods visible in browser (it skips Object methods). Select me to see all inherited methods which are not overriden by selected class'
]
