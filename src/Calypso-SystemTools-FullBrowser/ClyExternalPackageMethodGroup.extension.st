Extension { #name : #ClyExternalPackageMethodGroup }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyExternalPackageMethodGroup >> decorateTableCell: anItemCellMorph of: groupItem [
	super decorateTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph definitionIcon: #protocolExtensionIcon.
		
	(anItemCellMorph browser isPackagePartOfSelection: self package)
		ifTrue: [ anItemCellMorph label emphasis: TextEmphasis bold emphasisCode]
		ifFalse: [ anItemCellMorph label color: Smalltalk ui theme classExtensionColor].
]
