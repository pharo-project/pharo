"
A TaskbarMorphTest is a test class for testing the behavior of TaskbarMorph
"
Class {
	#name : #TaskbarMorphTest,
	#superclass : #TestCase,
	#instVars : [
		'taskbar',
		'world',
		'windows',
		'tasks'
	],
	#category : #'Morphic-Widgets-Taskbar-Tests'
}

{ #category : #'mocking world' }
TaskbarMorphTest >> addWindow [
	^world addWindowToWorld 
]

{ #category : #'mocking world' }
TaskbarMorphTest >> addWindowToWorld [
	| morph |
	morph := Morph new.
	windows add: morph. 
	^morph
]

{ #category : #'mocking taskbar' }
TaskbarMorphTest >> newTaskFor: window [
	^ TaskbarTask
		morph: window
		state: #aNilState
		icon: nil
		label: 'a Label'
]

{ #category : #'mocking taskbar' }
TaskbarMorphTest >> orderedTasks [
	^tasks 
]

{ #category : #'mocking taskbar' }
TaskbarMorphTest >> orderedTasks: aCollection [ 
	tasks:= aCollection
]

{ #category : #'mocking world' }
TaskbarMorphTest >> putOnTop: aMorph [ 
	windows remove: aMorph.
	windows addLast: aMorph
]

{ #category : #'mocking world' }
TaskbarMorphTest >> removeWindow: aMorph [ 
	windows remove: aMorph 
]

{ #category : #running }
TaskbarMorphTest >> setUp [ 
	super setUp.
	windows := OrderedCollection new.
	tasks := OrderedCollection new.
	self theMethodInProdThatShouldBeTested.
]

{ #category : #'mocking taskbar' }
TaskbarMorphTest >> tasks [
	^tasks
]

{ #category : #tests }
TaskbarMorphTest >> testEmptyTaskBarList [
	taskbar := self.
	world := self.
	taskbar updateTasks.
	self assert: taskbar tasks size equals: 0
	
]

{ #category : #tests }
TaskbarMorphTest >> testIfTheTestedMethodIstheSameThatTheOneUsedInProd [

	self assert: self theMethodInProdThatShouldBeTested bytecode equals: (self class >> #updateOrderedTasksFrom:) bytecode
]

{ #category : #tests }
TaskbarMorphTest >> testUpdateEmptyTaskBarWithNewWindow [
	taskbar := self.
	world := self.
	world addWindow.
	taskbar updateTasks.
	self assert: taskbar tasks size equals: 1
	
]

{ #category : #tests }
TaskbarMorphTest >> testUpdateTaskBarShouldContainsTaskbarTasks [
	taskbar := self.
	world := self.
	world addWindow.
	taskbar updateTasks.
	self assert: (taskbar tasks first isKindOf: TaskbarTask)
	
]

{ #category : #tests }
TaskbarMorphTest >> testUpdateTaskBarShouldContainsTaskbarTasksForEachWindow [
	| window1 window2 |
	taskbar := self.
	world := self.
	window1 := world addWindow.
	window2 := world addWindow.
	taskbar updateTasks.
	self assert: taskbar tasks first morph equals: window1.
	self assert: taskbar tasks second morph equals: window2
]

{ #category : #tests }
TaskbarMorphTest >> testWhenChangingOrderInWorldListTaskOrderShouldRemains [
	| window1 window2 |
	taskbar := self.
	world := self.
	window1 := world addWindow.
	window2 := world addWindow.
	taskbar updateTasks.
	world putOnTop: window1.
	taskbar updateTasks.
	self assert: taskbar tasks first morph equals: window1.
	self assert: taskbar tasks second morph equals: window2
]

{ #category : #tests }
TaskbarMorphTest >> testWhenRemovingWindowInWorldListTaskListShouldUpdateConsequently [
	| window1 window2 |
	taskbar := self.
	world := self.
	window1 := world addWindow.
	window2 := world addWindow.
	taskbar updateTasks.
	world removeWindow: window1.
	taskbar updateTasks.
	self assert: taskbar tasks size equals: 1.
	self assert: taskbar tasks first morph equals: window2.
	
]

{ #category : #'the real thing - aka: the test subject' }
TaskbarMorphTest >> theMethodInProdThatShouldBeTested [
	^ TaskbarMorph >> #updateOrderedTasksFrom:
]

{ #category : #'the real thing - aka: the test subject' }
TaskbarMorphTest >> updateOrderedTasksFrom: tasksThatShouldBeUpdated [
	| deadTasks |
	deadTasks := OrderedCollection new.
	self orderedTasks
		do: [ :aTaskbarTask | 
			tasksThatShouldBeUpdated
				detect:
					[ :aTaskThatShouldBeUpdated | aTaskThatShouldBeUpdated morph = aTaskbarTask morph ]
				ifFound: [ :foundTask | tasksThatShouldBeUpdated remove: foundTask ]
				ifNone: [ deadTasks add: aTaskbarTask ] ].
	(deadTasks isEmpty and: [ tasksThatShouldBeUpdated isEmpty ])
		ifTrue: [ ^ self ].
	self orderedTasks: (self orderedTasks 
		removeAll: deadTasks;
		addAll: tasksThatShouldBeUpdated;
		yourself)
]

{ #category : #'mocking taskbar' }
TaskbarMorphTest >> updateTasks [
	| tasksThatShouldBeUpdated  |
	tasksThatShouldBeUpdated := windows
		collect: [ :window | self newTaskFor: window ].
	self updateOrderedTasksFrom: tasksThatShouldBeUpdated
	
]
