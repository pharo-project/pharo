"
A PDPackageAnalyzerWindow class is an abstract class.
It represents the UI merging the SpecTreeModel (package dependencies or cycles) and Nautilus UI.

The main feature is to browse automatically in the Nautilus when you click on a node (package or dependency).

"
Class {
	#name : #DAPackageAnalyzerWindow,
	#superclass : #StandardWindow,
	#instVars : [
		'packageAnalyzerTreeModel',
		'nautilus'
	],
	#category : 'Tool-DependencyAnalyser-UI-Core'
}

{ #category : #instance-creation }
DAPackageAnalyzerWindow class >> onPackagesNamed: aCollection [
	^ self new 
		initializeWithPackageNames: aCollection;
		yourself
]

{ #category : #accesing }
DAPackageAnalyzerWindow class >> selectedPackagesFrom: aBuilder [
	| target selectedPackages |
	target := aBuilder model.
	selectedPackages := target selectedPackages collect: [ :rpackage | rpackage packageName asString ].
	^ selectedPackages
]

{ #category : #examples }
DAPackageAnalyzerWindow class >> systemPackages [
	^ RPackageOrganizer default packages collect: [ :package | package packageName asString ]
]

{ #category : #adding }
DAPackageAnalyzerWindow >> addAllMorph [
	| windowNautilus  windowTreeModel |
	windowNautilus := self buildNautilusWindow.	
	windowTreeModel := self buildMorphWindowForTreeModel.
	packageAnalyzerTreeModel packageTreeNautilusUI: self nautilusUI.
	self
		addMorph: windowNautilus frame: (0.4 @ 0 corner: 1 @ 1);
		addMorph:  (self panelMorphFrom: windowTreeModel) frame: (0 @ 0 corner: 0.4 @ 1).
]

{ #category : #building }
DAPackageAnalyzerWindow >> buildMorphWindowForTreeModel [
	| specWindow |
	packageAnalyzerTreeModel
		open;
		delete.
	
	specWindow := self specWindowFrom: packageAnalyzerTreeModel.
	^ specWindow
]

{ #category : #building }
DAPackageAnalyzerWindow >> buildNautilusWindow [
	| nautilusUI nautilusWindow |

	nautilusUI := NautilusUI on: nautilus.
	nautilus instVarNamed: 'ui' put: nautilusUI.
	nautilusWindow := NautilusWindow new model: nautilusUI.
	nautilusUI 
		addAll: nautilusWindow;
		window: nautilusWindow;
		setWindowTitle.
	^ nautilusWindow
]

{ #category : #initialization-filter }
DAPackageAnalyzerWindow >> buildandAddAllMorph [
	nautilus := packageAnalyzerTreeModel packageTreeNautilus.
	self addAllMorph
]

{ #category : #'morphic ui' }
DAPackageAnalyzerWindow >> defaultLabel [
	^ 'Package Dependencies Analysis'
]

{ #category : #open/close }
DAPackageAnalyzerWindow >> delete [
	self packageAnalyzerTreeModel close.
	^ super delete
]

{ #category : #'morphic ui' }
DAPackageAnalyzerWindow >> initialExtent [
	^ (1500@800)
]

{ #category : #accessing }
DAPackageAnalyzerWindow >> nautilus [
	^ nautilus
]

{ #category : #accessing }
DAPackageAnalyzerWindow >> nautilusUI [
	^ nautilus ui
]

{ #category : #accessing }
DAPackageAnalyzerWindow >> open [
	^ self openInWorld.
	
]

{ #category : #accessing }
DAPackageAnalyzerWindow >> packageAnalyzerTreeModel [
	^ packageAnalyzerTreeModel
]

{ #category : #adding }
DAPackageAnalyzerWindow >> panelMorphFrom: aSpecWindow [
	aSpecWindow submorphs
		do: [ :submorph | 
			(submorph isKindOf: PanelMorph)
				ifTrue: [ ^ submorph ] ].
	^ nil
]

{ #category : #building }
DAPackageAnalyzerWindow >> specWindowFrom: aSpec [
	| windowPresenter specWindow |
	windowPresenter := aSpec window.
	specWindow := windowPresenter window.
	^ specWindow
]
