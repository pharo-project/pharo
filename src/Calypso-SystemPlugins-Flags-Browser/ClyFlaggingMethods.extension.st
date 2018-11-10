Extension { #name : #ClyFlaggingMethods }

{ #category : #'*Calypso-SystemPlugins-Flags-Browser' }
ClyFlaggingMethods >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	| icon |
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	icon := anItemCellMorph definitionIcon: #flagIcon.
	icon setBalloonText: 'There are methods with flag'
]

{ #category : #'*Calypso-SystemPlugins-Flags-Browser' }
ClyFlaggingMethods >> decorateResultMethodEditor: aMethodEditor [

	aMethodEditor selectAnySelector: #(flag:)
]
