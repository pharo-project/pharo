Class {
	#name : 'TaskMorphTest',
	#superclass : 'TestCase',
	#instVars : [
		'tasks',
		'windows'
	],
	#category : 'Morphic-Widgets-Taskbar-Tests',
	#package : 'Morphic-Widgets-Taskbar-Tests'
}

{ #category : 'mocking taskbar' }
TaskMorphTest >> newTaskFor: window [

	^ TaskbarTask
		  morph: window
		  state: #aNilState
		  icon: nil
		  label: 'a Label'
]

{ #category : 'builders' }
TaskMorphTest >> newTaskWithLabel: label [

	^ TaskbarTask
		morph: Morph new
		state: #aNilState
		icon: nil
		label: label
]

{ #category : 'builders' }
TaskMorphTest >> newTasks: numberOfTasks [

	^ self newTasks: numberOfTasks labelled: 'Task'
]

{ #category : 'builders' }
TaskMorphTest >> newTasks: numberOfTasks labelled: label [

	^ (1 to: numberOfTasks) collect: [ :i | self newTaskWithLabel: label, ' ', i printString ]
]

{ #category : 'mocking taskbar' }
TaskMorphTest >> registerTaskFor: window [

	| task |

	task := self newTaskFor: window.
	windows add: window.
	tasks add: task.

	^ task
]

{ #category : 'running' }
TaskMorphTest >> setUp [

	super setUp.
	windows := OrderedCollection new.
	tasks := OrderedCollection new
]

{ #category : 'acceas' }
TaskMorphTest >> task1 [

	^ tasks first
]

{ #category : 'accessing' }
TaskMorphTest >> task2 [

	^ tasks second
]

{ #category : 'accessing' }
TaskMorphTest >> task3 [

	^ tasks third
]

{ #category : 'builders' }
TaskMorphTest >> taskListWithTasks: someTasks [ 

	| taskList |

	taskList := TaskListMorph new.
	taskList removeAllMorphs.
	taskList tasks: someTasks asOrderedCollection.

	^ taskList
]
