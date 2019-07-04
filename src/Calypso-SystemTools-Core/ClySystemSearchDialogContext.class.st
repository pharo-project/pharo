Class {
	#name : #ClySystemSearchDialogContext,
	#superclass : #ClySystemBrowserContext,
	#category : #'Calypso-SystemTools-Core-Contexts'
}

{ #category : #accessing }
ClySystemSearchDialogContext >> browser [
	^tool browser
]

{ #category : #'selection-classes' }
ClySystemSearchDialogContext >> selectedClassItems [
	^self selectedItemsOfType: ClyClass
]

{ #category : #private }
ClySystemSearchDialogContext >> selectedItemsOfType: itemTypeClass [
	
	^selectedItems select: [ :each | 
		each isBasedOnItemType: itemTypeClass ]
]

{ #category : #'selection-methods' }
ClySystemSearchDialogContext >> selectedMethodItems [
	^self selectedItemsOfType: ClyMethod
]

{ #category : #'selection-packages' }
ClySystemSearchDialogContext >> selectedPackageItems [
	^self selectedItemsOfType: RPackage
]
