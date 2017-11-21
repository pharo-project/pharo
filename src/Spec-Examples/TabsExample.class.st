"
I am an example of how to use tabs reusing DynamicWidgetChange, ClassMethodBrowser and MessageBrowser, and using an EyeInspector.

self example
"
Class {
	#name : #TabsExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'manager'
	],
	#category : #Spec-Examples
}

{ #category : #specs }
TabsExample class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		add: #manager;
		yourself
]

{ #category : #examples }
TabsExample class >> example [
	^ self open
]

{ #category : #'instance creation' }
TabsExample class >> open [
	^self new openWithSpec
]

{ #category : #private }
TabsExample >> browserTab [
	| tab |
	tab := self newTab.
	tab
		label: 'Browser';
		icon: (tab iconNamed: #nautilusIcon);
		model:
			(ClassMethodBrowser new
				classes: Smalltalk allClasses;
				yourself).
	^ tab
]

{ #category : #private }
TabsExample >> dynamicTab [
      | tab |
      tab := self newTab.
      tab
          label: 'Dynamic';
          icon: (tab iconNamed: #nautilusIcon);
          model: (DynamicWidgetChange  new).
      ^ tab 
]

{ #category : #initialization }
TabsExample >> initializeWidgets [

	manager := self newTabManager.
	manager whenTabSelected: [ self updateTitle ].
	self populateManager
]

{ #category : #accessing }
TabsExample >> manager [

	^ manager
]

{ #category : #private }
TabsExample >> objectClassTab [
	| tab |
	tab := self newTab.
	tab
		label: 'Object class';
		icon: (tab iconNamed: #nautilusIcon);
		model: (MessageBrowser new messages: Object methods).
	^ tab
]

{ #category : #private }
TabsExample >> objectInspectorTab [
	|tab|
	tab := self newTab.
	tab
		label: 'Object inspector';
		icon: (tab iconNamed: #nautilusIcon);
		retrievingBlock: [ (EyeInspector new
				inspect: Object;
				buildWithSpec: #inspectorSpec) asWidget ].
			^tab
]

{ #category : #initialization }
TabsExample >> populateManager [

	manager
		addTab: self objectClassTab;
		addTab: self objectInspectorTab;
		addTab: self browserTab;
		addTab: self dynamicTab
]

{ #category : #accessing }
TabsExample >> title [

	^ self manager selectedTab
		ifNil: [ 'Tabs Example' ]
		ifNotNil: [:tab | tab label ]
]
