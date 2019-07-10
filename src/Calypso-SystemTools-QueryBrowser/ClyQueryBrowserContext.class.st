"
I represent query browser context
"
Class {
	#name : #ClyQueryBrowserContext,
	#superclass : #ClySystemBrowserContext,
	#category : #'Calypso-SystemTools-QueryBrowser-UI'
}

{ #category : #'selection strategy' }
ClyQueryBrowserContext class >> selectionStrategy [
	<classAnnotation>
	
	^ClyContextSelectionStrategy for: ClyQueryBrowser
]

{ #category : #'selection-classes' }
ClyQueryBrowserContext >> isClassSelected [
	"here we extract selected classes from selected items"
	^self hasSelectedItems 
]

{ #category : #'selection-methods' }
ClyQueryBrowserContext >> isMethodSelected [
	"optimized version without extra collection copy"
	
	^selectedItems anySatisfy: [ :each | each isBasedOnItemType: ClyMethod  ]
]

{ #category : #'selection-packages' }
ClyQueryBrowserContext >> isPackageSelected [
	"here we extract selected packages from selected items"
	^self hasSelectedItems 
]

{ #category : #'selection-classes' }
ClyQueryBrowserContext >> lastSelectedClass [
	
	^self lastSelectedItem systemDefinition definingClass
]

{ #category : #'selection-packages' }
ClyQueryBrowserContext >> lastSelectedPackage [
	
	^self lastSelectedItem systemDefinition definingPackage
]

{ #category : #'browser state' }
ClyQueryBrowserContext >> restoreBrowserState [

	self browser selectObject: self lastSelectedObject
]

{ #category : #'selection-classes' }
ClyQueryBrowserContext >> selectedClassItems [
	
	| unique classItem |
	unique := OrderedCollection new.
	selectedItems do: [ :each | 
		classItem := each systemDefinition definingClassItem.
		unique 
			detect: [:existing | existing actualObject == classItem actualObject] 
			ifNone: [unique add: classItem]].
	^unique
]

{ #category : #'selection-classes' }
ClyQueryBrowserContext >> selectedClassSide [
	^self lastSelectedClass
]

{ #category : #'selection-methods' }
ClyQueryBrowserContext >> selectedMethodItems [
	^selectedItems select: [ :each | each isBasedOnItemType: ClyMethod ]
]

{ #category : #'selection-packages' }
ClyQueryBrowserContext >> selectedPackageItems [
	
	| unique packageItem |
	unique := OrderedCollection new.
	selectedItems do: [ :each | 
		packageItem := each systemDefinition definingPackageItem.
		unique 
			detect: [:existing | existing actualObject == packageItem actualObject] 
			ifNone: [unique add: packageItem]].
	^unique
]
