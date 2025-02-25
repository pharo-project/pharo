"
A TaskbarMorphTest is a test class for testing the behavior of TaskbarMorph
"
Class {
	#name : 'TaskbarMorphTest',
	#superclass : 'TestCase',
	#instVars : [
		'taskbar',
		'world',
		'windows',
		'tasks'
	],
	#category : 'Morphic-Widgets-Taskbar-Tests',
	#package : 'Morphic-Widgets-Taskbar-Tests'
}

{ #category : 'mocking taskbar' }
TaskbarMorphTest >> addTaskWithWindow [

	| newTask morph |
	morph := SystemWindow new.
	newTask := taskbar newTaskFor: morph.
	self orderedTasks add: newTask.
	^ newTask
]

{ #category : 'mocking world' }
TaskbarMorphTest >> addWindow [
	^world addWindowToWorld
]

{ #category : 'mocking world' }
TaskbarMorphTest >> addWindowToWorld [
	| morph |
	morph := Morph new.
	windows add: morph.
	^morph
]

{ #category : 'mocking taskbar' }
TaskbarMorphTest >> newTaskFor: window [
	^ TaskbarTask
		morph: window
		state: #aNilState
		icon: nil
		label: 'a Label'
]

{ #category : 'mocking taskbar' }
TaskbarMorphTest >> orderedTasks [
	^tasks
]

{ #category : 'mocking taskbar' }
TaskbarMorphTest >> orderedTasks: aCollection [
	tasks:= aCollection
]

{ #category : 'mocking world' }
TaskbarMorphTest >> putOnTop: aMorph [
	windows remove: aMorph.
	windows addLast: aMorph
]

{ #category : 'mocking world' }
TaskbarMorphTest >> removeWindow: aMorph [
	windows remove: aMorph
]

{ #category : 'running' }
TaskbarMorphTest >> setUp [
	super setUp.
	windows := OrderedCollection new.
	tasks := OrderedCollection new.
	self theMethodInProdThatShouldBeTested
]

{ #category : 'mocking taskbar' }
TaskbarMorphTest >> tasks [
	^tasks
]

{ #category : 'tests' }
TaskbarMorphTest >> testCanBeMovedLeftIfItsNotTheFirstTask [

	| firstTask secondTask taskbarMorph |
	taskbar := self.
	world := self.
	taskbar updateTasks.
	taskbarMorph := TaskbarMorph new.
	firstTask := self addTaskWithWindow.
	secondTask := self addTaskWithWindow.
	taskbarMorph orderedTasks: {
			firstTask.
			secondTask } asOrderedCollection.

	self assert: (taskbarMorph canMoveLeft: secondTask morph)
]

{ #category : 'tests' }
TaskbarMorphTest >> testCanBeMovedRightIfItsNotTheLastTask [

	| firstTask secondTask taskbarMorph |
	taskbar := self.
	world := self.
	taskbar updateTasks.
	taskbarMorph := TaskbarMorph new.
	firstTask := self addTaskWithWindow.
	secondTask := self addTaskWithWindow.
	taskbarMorph orderedTasks: {
			firstTask.
			secondTask } asOrderedCollection.

	self assert: (taskbarMorph canMoveRight: firstTask morph)
]

{ #category : 'tests' }
TaskbarMorphTest >> testCantBeMovedLeftIfItsTheFirstTask [

	| firstTask secondTask taskbarMorph |
	taskbar := self.
	world := self.
	taskbar updateTasks.
	taskbarMorph := TaskbarMorph new.
	firstTask := self addTaskWithWindow.
	secondTask := self addTaskWithWindow.
	taskbarMorph orderedTasks: {
			firstTask.
			secondTask } asOrderedCollection.

	self deny: (taskbarMorph canMoveLeft: firstTask morph)
]

{ #category : 'tests' }
TaskbarMorphTest >> testCantBeMovedRightIfItsTheLastTask [

	| firstTask secondTask taskbarMorph |
	taskbar := self.
	world := self.
	taskbar updateTasks.
	taskbarMorph := TaskbarMorph new.
	firstTask := self addTaskWithWindow.
	secondTask := self addTaskWithWindow.
	taskbarMorph orderedTasks: {
			firstTask.
			secondTask } asOrderedCollection.


	self
		deny: (taskbarMorph canMoveRight: secondTask morph)
]

{ #category : 'tests' }
TaskbarMorphTest >> testEmptyTaskBarList [
	taskbar := self.
	world := self.
	taskbar updateTasks.
	self assert: taskbar tasks size equals: 0
]

{ #category : 'tests' }
TaskbarMorphTest >> testIfTheTestedMethodIstheSameThatTheOneUsedInProd [

	self assert: self theMethodInProdThatShouldBeTested bytecodes equals: (self class >> #updateOrderedTasksFrom:) bytecodes
]

{ #category : 'tests' }
TaskbarMorphTest >> testMoveTaskbarTaskLeftDoesNothingOnFirstTask [
	| firstTask taskbarMorph |
	taskbar := self.
	world := self.
	taskbar updateTasks.
	taskbarMorph := TaskbarMorph new.
	firstTask := self addTaskWithWindow.
	taskbarMorph orderedTasks: { firstTask } asOrderedCollection.
	taskbarMorph move: firstTask withOffset: -1.

	self assert: firstTask equals: taskbarMorph orderedTasks first
]

{ #category : 'tests' }
TaskbarMorphTest >> testMoveTaskbarTaskLeftMovesItOnePositionToTheLeft [
	| firstTask secondTask taskbarMorph |
	taskbar := self.
	world := self.
	taskbar updateTasks.
	taskbarMorph := TaskbarMorph new.
	firstTask := self addTaskWithWindow.
	secondTask := self addTaskWithWindow.
	taskbarMorph orderedTasks: { firstTask. secondTask } asOrderedCollection.
	taskbarMorph move: secondTask withOffset: -1.

	self assert: secondTask equals: taskbarMorph orderedTasks first
]

{ #category : 'tests' }
TaskbarMorphTest >> testMoveTaskbarTaskRightDoesNothingOnLastTask [
	| firstTask taskbarMorph |
	taskbar := self.
	world := self.
	taskbar updateTasks.
	taskbarMorph := TaskbarMorph new.
	firstTask := self addTaskWithWindow.
	taskbarMorph orderedTasks: { firstTask } asOrderedCollection.
	taskbarMorph move: firstTask withOffset: 1.

	self assert: firstTask equals: taskbarMorph orderedTasks first
]

{ #category : 'tests' }
TaskbarMorphTest >> testMoveTaskbarTaskRightMovesItOnePositionToTheRight [
	| firstTask secondTask taskbarMorph |
	taskbar := self.
	world := self.
	taskbar updateTasks.
	taskbarMorph := TaskbarMorph new.
	firstTask := self addTaskWithWindow.
	secondTask := self addTaskWithWindow.
	taskbarMorph orderedTasks: { firstTask. secondTask } asOrderedCollection.
	taskbarMorph move: firstTask withOffset: 1.

	self assert: firstTask equals: taskbarMorph orderedTasks last
]

{ #category : 'tests' }
TaskbarMorphTest >> testUpdateEmptyTaskBarWithNewWindow [
	taskbar := self.
	world := self.
	world addWindow.
	taskbar updateTasks.
	self assert: taskbar tasks size equals: 1
]

{ #category : 'tests' }
TaskbarMorphTest >> testUpdateTaskBarShouldContainsTaskbarTasks [
	taskbar := self.
	world := self.
	world addWindow.
	taskbar updateTasks.
	self assert: (taskbar tasks first isKindOf: TaskbarTask)
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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

{ #category : 'tests' }
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
	self assert: taskbar tasks first morph equals: window2
]

{ #category : 'the real thing - aka: the test subject' }
TaskbarMorphTest >> theMethodInProdThatShouldBeTested [
	^ TaskbarMorph >> #updateOrderedTasksFrom:
]

{ #category : 'the real thing - aka: the test subject' }
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

{ #category : 'mocking taskbar' }
TaskbarMorphTest >> updateTasks [
	| tasksThatShouldBeUpdated  |
	tasksThatShouldBeUpdated := windows
		collect: [ :window | self newTaskFor: window ].
	self updateOrderedTasksFrom: tasksThatShouldBeUpdated
]
