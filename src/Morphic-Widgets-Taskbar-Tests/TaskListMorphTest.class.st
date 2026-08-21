Class {
	#name : 'TaskListMorphTest',
	#superclass : 'TaskMorphTest',
	#instVars : [
		'taskList'
	],
	#category : 'Morphic-Widgets-Taskbar-Tests',
	#package : 'Morphic-Widgets-Taskbar-Tests'
}

{ #category : 'running' }
TaskListMorphTest >> setUp [

	super setUp.
	self registerTaskFor: SystemWindow new.
	self registerTaskFor: SystemWindow new.
	self registerTaskFor: SystemWindow new.

	taskList := TaskListMorph new tasks: tasks
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testContainerOverhead [

	self assert: taskList containerOverhead equals: 16
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testContainerPadding [

	self assert: taskList containerPadding equals: 100
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testEffectivePreviewExtentRespectsFloor [

	| aTaskList extent minX |

	aTaskList := self taskListWithTasks: (self newTasks: 200).
	extent := aTaskList effectivePreviewExtent.
	minX := (aTaskList defaultPreviewExtent x * 0.2) asInteger.

	self assert: extent x >= minX
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testEffectivePreviewExtentReturnsDefaultForFewTasks [

	| aTaskList previousWorldWidth |
	
	previousWorldWidth := self currentWorld width.
	self currentWorld width: 1800.
	
	aTaskList := self taskListWithTasks: (self newTasks: 3).
	self assert: aTaskList effectivePreviewExtent equals: aTaskList defaultPreviewExtent.
	
	self currentWorld width: previousWorldWidth.
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testEffectivePreviewExtentScalesForManyTasks [

	| aTaskList |

	aTaskList := self taskListWithTasks: (self newTasks: 30).

	self assert: (aTaskList effectivePreviewExtent) x < aTaskList defaultPreviewExtent x
]

{ #category : 'tests' }
TaskListMorphTest >> testHasAllWindows [

	| newTask |

	self assert: (taskList hasCurrentWindows: tasks).

	newTask := self newTaskFor: SystemWindow new.
	self deny: (taskList hasCurrentWindows: tasks , { newTask }).

	self deny: (taskList hasCurrentWindows: tasks allButLast)
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testLabelFontSizeIsScaled [

	| aTaskList container labelFont |

	aTaskList := self taskListWithTasks: (self newTasks: 30).
	aTaskList updateButtonsAndPreview.
	container := aTaskList submorphs first.
	labelFont := (container submorphs second) font.

	self assert: labelFont pointSize < 11.
	self assert: labelFont pointSize >= 8
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testLabelShowsFullTextWhenShorterThanLimit [

	| aTaskList task label |
	
	aTaskList := self taskListWithTasks: (self newTasks: 1).
	task := self newTaskWithLabel: 'Short'.
	
	label := aTaskList label: task truncation: 10 size: 9.
	self assert: label contents equals: 'Short'
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testLabelTruncatesWhenLongerThanLimit [

	| aTaskList task label |
	
	aTaskList := self taskListWithTasks: (self newTasks: 1).
	task := self newTaskWithLabel: 'A very long task name to truncate'.
	
	label := aTaskList label: task truncation: 8 size: 9.
	self assert: label contents size <= 8
]

{ #category : 'tests - max width' }
TaskListMorphTest >> testMaxWidthReturnsPositiveValue [

	| aTaskList |
	aTaskList := self taskListWithTasks: (self newTasks: 5).
	
	self assert: aTaskList maxWidth > 0
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

{ #category : 'tests - max width' }
TaskListMorphTest >> testUpdateTaskButtonsCreatesCorrectNumberOfSubmorphs [

	| aTaskList |
	
	aTaskList := self taskListWithTasks: (self newTasks: 7).
	aTaskList updateButtonsAndPreview.
	
	self assert: aTaskList submorphs size equals: 7
]
