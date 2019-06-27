"
I am an example of how to use tabs reusing DynamicWidgetChange, ClassMethodBrowser and MessageBrowser, and using an EyeInspector.

self example
"
Class {
	#name : #SpTabsExample,
	#superclass : #SpPresenter,
	#instVars : [
		'manager'
	],
	#category : #'Spec2-Deprecated-Replaced'
}

{ #category : #specs }
SpTabsExample class >> defaultSpec [
	<spec>
	
	^ SpLayout composed
		add: #manager;
		yourself
]

{ #category : #examples }
SpTabsExample class >> example [
	<sampleInstance>
	^ self open
]

{ #category : #deprecation }
SpTabsExample class >> isDeprecated [
	^ true
]

{ #category : #'instance creation' }
SpTabsExample class >> open [
	^self new openWithSpec
]

{ #category : #private }
SpTabsExample >> browserTab [
	^ self newTab
		label: 'Browser';
		icon: (self iconNamed: #nautilusIcon);
		presenter:
			(SpClassMethodBrowser new
				classes: Smalltalk allClasses;
				yourself);
		yourself
]

{ #category : #private }
SpTabsExample >> dynamicTab [
	^ self newTab
		label: 'Dynamic';
		icon: (self iconNamed: #nautilusIcon);
		presenter: SpDynamicWidgetChange new;
		yourself
]

{ #category : #api }
SpTabsExample >> initialExtent [
	^ 900@600
]

{ #category : #initialization }
SpTabsExample >> initializeWidgets [

	manager := self newTabManager.
	manager whenTabSelected: [ self updateTitle ].
	self populateManager
]

{ #category : #accessing }
SpTabsExample >> manager [

	^ manager
]

{ #category : #private }
SpTabsExample >> objectClassTab [
	^ self newTab
		label: 'Object class';
		icon: (self iconNamed: #nautilusIcon);
		presenter: (MessageBrowser new messages: Object methods);
		yourself
]

{ #category : #private }
SpTabsExample >> objectInspectorTab [
	^ self newTab
		label: 'Object inspector';
		icon: (self iconNamed: #nautilusIcon);
		retrievingBlock: [ SpEyeInspector new
				inspect: Object;
				buildWithSpec: #inspectorSpec ];
		yourself
]

{ #category : #initialization }
SpTabsExample >> populateManager [

	manager
		addTab: self objectClassTab;
		addTab: self objectInspectorTab;
		addTab: self browserTab;
		addTab: self dynamicTab
]

{ #category : #accessing }
SpTabsExample >> title [

	^ self manager selectedTab
		ifNil: [ 'Tabs Example' ]
		ifNotNil: [:tab | tab label ]
]
