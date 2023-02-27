"
I represent a tool in the System Browser when a Package is selected.
I display an extra-tab to show dependencies of the selected package.
"
Class {
	#name : #DABrowserToolMorph,
	#superclass : #ClyBrowserToolMorph,
	#instVars : [
		'presenter',
		'analyzedPackage'
	],
	#category : #'Tool-DependencyAnalyser-UI-Tab'
}

{ #category : #activation }
DABrowserToolMorph class >> packageActivation [
	<classAnnotation>
	^ ClyTabActivationStrategyAnnotation for: RPackage asCalypsoItemContext
]

{ #category : #accessing }
DABrowserToolMorph class >> tabOrder [
	^ -10
]

{ #category : #accessing }
DABrowserToolMorph >> analyzedPackage [
	^ analyzedPackage
]

{ #category : #building }
DABrowserToolMorph >> build [

	presenter := DATabPackageDependenciesPresenter on: analyzedPackage.
	presenter browser: self browser.
	self
		addMorph: presenter build
		fullFrame: LayoutFrame identity
]

{ #category : #private }
DABrowserToolMorph >> daPackageFor: aPackageName [

	^ DADependencyChecker new daPackageFor: aPackageName
]

{ #category : #initialization }
DABrowserToolMorph >> defaultIconName [

	^ #objects
]

{ #category : #initialization }
DABrowserToolMorph >> defaultTitle [

	^ 'Dependencies'
]

{ #category : #testing }
DABrowserToolMorph >> isSimilarTo: anotherBrowserTool [
	(super isSimilarTo: anotherBrowserTool) ifFalse: [ ^false ].
	^ self analyzedPackage = anotherBrowserTool analyzedPackage
]

{ #category : #testing }
DABrowserToolMorph >> isValidInContext: aClyFullBrowserContext [
	self context class = aClyFullBrowserContext class
		ifFalse: [ ^ false ].

	^ self analyzedPackage rPackageSet packageName = aClyFullBrowserContext lastSelectedClassGroup name
]

{ #category : #initialization }
DABrowserToolMorph >> setUpModelFromContext [

	analyzedPackage := self daPackageFor: context lastSelectedClassGroup name
]
