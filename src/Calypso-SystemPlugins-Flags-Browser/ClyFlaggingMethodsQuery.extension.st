Extension { #name : #ClyFlaggingMethodsQuery }

{ #category : #'*Calypso-SystemPlugins-Flags-Browser' }
ClyFlaggingMethodsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	| icon |
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	icon := anItemCellMorph definitionIcon: #flagIcon.
	icon setBalloonText: 'There are methods with flag'
]

{ #category : #'*Calypso-SystemPlugins-Flags-Browser' }
ClyFlaggingMethodsQuery >> decorateResultMethodEditor: aMethodEditor [

	aMethodEditor selectAnySelector: #(flag:)
]
