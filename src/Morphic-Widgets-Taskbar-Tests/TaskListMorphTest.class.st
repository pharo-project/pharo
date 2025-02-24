Class {
	#name : 'TaskListMorphTest',
	#superclass : 'TaskMorphTest',
	#instVars : [
		'taskList'
	],
	#category : 'Morphic-Widgets-Taskbar-Tests',
	#package : 'Morphic-Widgets-Taskbar-Tests'
}

{ #category : 'tests' }
TaskListMorphTest >> setUp [

	super setUp.
	self registerTaskFor: SystemWindow new.
	self registerTaskFor: SystemWindow new.
	self registerTaskFor: SystemWindow new.

	taskList := TaskListMorph new tasks: tasks
]

{ #category : 'tests' }
TaskListMorphTest >> testHasAllWindows [

	| newTask |
	self assert: (taskList hasCurrentWindows: tasks).

	newTask := self newTaskFor: SystemWindow new.
	self deny: (taskList hasCurrentWindows: tasks , { newTask }).

	self deny: (taskList hasCurrentWindows: tasks allButLast)
]

{ #category : 'tests' }
TaskListMorphTest >> testSelectNextTask [

	self assert: taskList activeTask equals: nil.

	self task1 state: #active.
	self assert: taskList activeTask equals: self task1.

	taskList selectNextTask.
	self assert: taskList activeTask equals: self task2.
	
	taskList selectNextTask.
	self assert: taskList activeTask equals: self task3.
	
	taskList selectNextTask. "cycle"
	self assert: taskList activeTask equals: self task1.
]

{ #category : 'tests' }
TaskListMorphTest >> testSelectPreviousTask [

	self assert: taskList activeTask equals: nil.

	self task3 state: #active.
	self assert: taskList activeTask equals: self task3.

	taskList selectPreviousTask.
	self assert: taskList activeTask equals: self task2.
	
	taskList selectPreviousTask.
	self assert: taskList activeTask equals: self task1.
	
	taskList selectPreviousTask. "cycle"
	self assert: taskList activeTask equals: self task3.
]
