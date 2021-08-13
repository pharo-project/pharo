"
A CBPackagesChooserPresenterTest is a test class for testing the behaivor of CBPackagesChooserPresenter
"
Class {
	#name : #CBSelectPackageTest,
	#superclass : #TestCase,
	#category : #'Tool-CriticBrowser-Tests'
}

{ #category : #running }
CBSelectPackageTest >> setUp [

	super setUp.
	
]

{ #category : #initialization }
CBSelectPackageTest >> testInitializePresenters [

	| selectPackage |
	
	selectPackage := CBSelectPackage new.
	self
		assert: selectPackage chooserPackages class
		equals: SpChooserPresenter.
	self
		assert: selectPackage nextButton class
		equals: SpButtonPresenter.
]

{ #category : #initialization }
CBSelectPackageTest >> testOpen [

	| window |
	window := CBSelectPackage open.
	self assert: window isBuilt.
	window close
]
