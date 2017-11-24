"
I am widget for selecting environments.
"
Class {
	#name : #EnvironmentChooser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'enableGlobal',
		'enableCustom',
		'labelView',
		'environmentsView',
		'environmentsHolder',
		'environmentHolder'
	],
	#category : #'NautilusRefactoring-Utilities'
}

{ #category : #specs }
EnvironmentChooser class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		newRow: [ :r| 
			r 
				add: #labelView width: 45; 
				add: #environmentsView ]
]

{ #category : #actions }
EnvironmentChooser >> chooseNewEnvironment [
	| packagesAndClasses packages classes packageEnvironment classEnvironment |
	
	packagesAndClasses := PackageAndClassChooser new chooseModalTo: self window.
	(packagesAndClasses isNil or: [ packagesAndClasses isEmpty])
		ifTrue: [ ^ nil ].
	
	self flag: 'TODO create proper environments in a less ugly way'.
	packages := OrderedCollection new.
	classes := OrderedCollection new.
	
	packagesAndClasses do: [ :each |
		(each isKindOf: RPackage)
			ifTrue: [ packages add: each ]
			ifFalse: [ classes add: each ]].
		
	packageEnvironment := RBPackageEnvironment packages: packages.
	classEnvironment := RBClassEnvironment classes: classes.
	
	classes ifEmpty: [ ^ packageEnvironment ].
	packages ifEmpty: [ ^ classEnvironment ].
	^ packageEnvironment | classEnvironment
]

{ #category : #accessing }
EnvironmentChooser >> enableCustom [
	^ enableCustom value
]

{ #category : #accessing }
EnvironmentChooser >> enableCustom: aBoolean [
	^ enableCustom value: aBoolean
]

{ #category : #accessing }
EnvironmentChooser >> enableGlobal [
	^ enableGlobal value
]

{ #category : #accessing }
EnvironmentChooser >> enableGlobal: aBoolean [
	^ enableGlobal value: aBoolean
]

{ #category : #accessing }
EnvironmentChooser >> environment [
	^ environmentHolder value
]

{ #category : #accessing }
EnvironmentChooser >> environment: anEnvironment [
	self environment = anEnvironment 
		ifFalse: [ environmentHolder value: anEnvironment ]
]

{ #category : #accessing }
EnvironmentChooser >> environments [
	^ environmentsHolder value
]

{ #category : #accessing }
EnvironmentChooser >> environments: aCollection [
	environmentsHolder value: aCollection
]

{ #category : #accessing }
EnvironmentChooser >> environmentsView [
	^ environmentsView ifNil: [ 
		environmentsView := self instantiate: DropListPresenter. ]
]

{ #category : #initialization }
EnvironmentChooser >> initialize [
	enableCustom := true asValueHolder.
	enableGlobal := true asValueHolder.
	environmentsHolder := #() asValueHolder.
	environmentHolder := nil asValueHolder.
	super initialize.
]

{ #category : #initialization }
EnvironmentChooser >> initializeWidgets [
	environmentsHolder whenChangedDo: [ self update ].
	enableCustom whenChangedDo: [ self update ].
	enableGlobal whenChangedDo: [ self update ].
	self update.
]

{ #category : #accessing }
EnvironmentChooser >> label [
	^ self labelView label
]

{ #category : #accessing }
EnvironmentChooser >> label: aString [
	self labelView label: aString
]

{ #category : #accessing }
EnvironmentChooser >> labelView [
	^ labelView ifNil: [ 
		labelView := self instantiate: LabelPresenter.
		labelView label: 'Environment:' ]
]

{ #category : #updating }
EnvironmentChooser >> update [
	self environmentsView
		emptyList.
		
	self enableGlobal ifTrue: [ 
		self environmentsView addItemLabeled: 'System-wide' do: [ 
			self environment: RBBrowserEnvironment new ]].
	
	self environments do: [ :environemnt| |capturedValue|
		capturedValue := environemnt.
		self environmentsView
			addItemLabeled: capturedValue label
			do: [ self environment: capturedValue ]].
	
	self enableCustom ifTrue: [ 
		self environmentsView addItemLabeled: 'Custom...' do: [
			self chooseNewEnvironment ifNotNil: [ :newEnvironment | 
				self environment: newEnvironment ]]].
]

{ #category : #events }
EnvironmentChooser >> whenEnvironmentChanged: aBlock [
	environmentHolder whenChangedDo: aBlock
]
