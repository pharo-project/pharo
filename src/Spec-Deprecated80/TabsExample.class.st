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
	#category : #'Spec-Deprecated80-Replaced'
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
	<sampleInstance>
	^ self open
]

{ #category : #'instance creation' }
TabsExample class >> open [
	^self new openWithSpec
]

{ #category : #private }
TabsExample >> browserTab [
	^ self newTab
		label: 'Browser';
		icon: (self iconNamed: #nautilusIcon);
		presenter:
			(ClassMethodBrowser new
				classes: Smalltalk allClasses;
				yourself);
		yourself
]

{ #category : #private }
TabsExample >> dynamicTab [
	^ self newTab
		label: 'Dynamic';
		icon: (self iconNamed: #nautilusIcon);
		presenter: DynamicWidgetChange new;
		yourself
]

{ #category : #api }
TabsExample >> initialExtent [
	^ 900@600
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
	^ self newTab
		label: 'Object class';
		icon: (self iconNamed: #nautilusIcon);
		presenter: (MessageBrowser new messages: Object methods);
		yourself
]

{ #category : #private }
TabsExample >> objectInspectorTab [
	^ self newTab
		label: 'Object inspector';
		icon: (self iconNamed: #nautilusIcon);
		retrievingBlock: [ EyeInspector new
				inspect: Object;
				buildWithSpec: #inspectorSpec ];
		yourself
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
