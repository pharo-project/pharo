"
This plugin shows the tasks in the current package.

A task is a method which sends #flag: to mark it as #todo, or #fixme, or whatever task you want to fix.
Clicking in the specific task makes you jump to the marked method.
"
Class {
	#name : #PackageTasksPlugin,
	#superclass : #AbstractPackageSelectedPlugin,
	#instVars : [
		'tasks',
		'index',
		'panel'
	],
	#category : #NautilusCommon-Plugin
}

{ #category : #position }
PackageTasksPlugin class >> defaultPosition [

	^ #top
]

{ #category : #position }
PackageTasksPlugin class >> description [
	^self comment
]

{ #category : #display }
PackageTasksPlugin >> buildTaskList [
	^ (PluggableListMorph
		on: self
		list: #tasks
		selected: #selectedIndex
		changeSelected: #selectedIndex:
		menu: nil
		keystroke: nil)
		height: 50;
		hResizing: #spaceFill;
		yourself
]

{ #category : #display }
PackageTasksPlugin >> display [
	^panel
]

{ #category : #display }
PackageTasksPlugin >> index [
	^index
]

{ #category : #initialization }
PackageTasksPlugin >> initialize [
	tasks := OrderedCollection new.
	index := 0.
	panel := PanelMorph new
				changeTableLayout;
				listDirection: #leftToRight;
				addMorph: self buildTaskList;
				addMorph: ((StringMorph contents: 'Tasks:') width: 50; height: 50; yourself);
				hResizing: #spaceFill;
				vResizing: #shrinkWrap;
				yourself.

]

{ #category : #announcement }
PackageTasksPlugin >> packageSelected: anAnnouncement [
	
	| package |
	package := anAnnouncement package package.
	package
		ifNil: [ tasks removeAll ]
		ifNotNil: [ tasks := (self systemNavigation allCallsOn: #flag:) 
				select: [ :m | package includesMethod: m selector ofClass: m methodClass ] ].
	
	index := 0.	
	self changed: #tasks
]

{ #category : #display }
PackageTasksPlugin >> selectedIndex [
	^index
]

{ #category : #display }
PackageTasksPlugin >> selectedIndex: anIndex [
	index := anIndex.
	self updateMethodSelection: (tasks at: index ifAbsent: [ ^self ]).
	self changed: #selectedIndex
]

{ #category : #display }
PackageTasksPlugin >> tasks [
	^tasks
]

{ #category : #display }
PackageTasksPlugin >> updateMethodSelection: aMethod [

	model ui categoryWidget
		resetCategoryList;
		resetCategorySelection.

	model ui categoryWidget
		categorySelectionNamed: aMethod category put: true.

	model ui methodWidget 
		resetMethodList;
		resetMethodSelection.
		
	model ui methodWidget 
		selectMethod: aMethod.

	model ui resetClassesListSelection.
	model ui classWidget selectClass: aMethod methodClass.
	model  
		package: model selectedPackage 
		class: aMethod methodClass 
		protocol: aMethod category 
		method: aMethod method.
		
	model ui 
		updateClassView;
		updateCategoryAndMethodList;
		updateOnClassSelection
	
]
