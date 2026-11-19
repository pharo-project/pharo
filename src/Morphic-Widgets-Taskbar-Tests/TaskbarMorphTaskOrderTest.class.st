"
I check that a taskbar moves the tasks of its windows, in a world of my own.
"
Class {
	#name : 'TaskbarMorphTaskOrderTest',
	#superclass : 'TestCase',
	#instVars : [
		'world',
		'first',
		'second',
		'taskbar'
	],
	#category : 'Morphic-Widgets-Taskbar-Tests',
	#package : 'Morphic-Widgets-Taskbar-Tests'
}

{ #category : 'running' }
TaskbarMorphTaskOrderTest >> setUp [

	super setUp.
	world := WorldMorph new.
	first := SystemWindow labelled: 'first'.
	second := SystemWindow labelled: 'second'.
	world
		addMorph: first;
		addMorph: second.
	taskbar := TaskbarMorph new.
	world addMorph: taskbar.
	taskbar updateTasks
]

{ #category : 'tests' }
TaskbarMorphTaskOrderTest >> testCannotMoveTheFirstTaskLeft [

	self deny: (taskbar canMoveLeft: self windowsInTaskOrder first).
	self assert: (taskbar canMoveRight: self windowsInTaskOrder first)
]

{ #category : 'tests' }
TaskbarMorphTaskOrderTest >> testMoveTaskLeft [

	| before |
	before := self windowsInTaskOrder.

	taskbar moveTaskLeftOf: before last.

	self assert: self windowsInTaskOrder equals: before reversed
]

{ #category : 'tests' }
TaskbarMorphTaskOrderTest >> testMoveTaskRight [

	| before |
	before := self windowsInTaskOrder.

	taskbar moveTaskRightOf: before first.

	self assert: self windowsInTaskOrder equals: before reversed
]

{ #category : 'tests' }
TaskbarMorphTaskOrderTest >> testTasksAreTheWindowsOfTheWorld [

	self assert: self windowsInTaskOrder asSet equals: {
		first.
		second } asSet
]

{ #category : 'helpers' }
TaskbarMorphTaskOrderTest >> windowsInTaskOrder [

	^ taskbar orderedTasks collect: [ :task | task morph ] as: Array
]
