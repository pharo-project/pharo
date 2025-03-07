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
