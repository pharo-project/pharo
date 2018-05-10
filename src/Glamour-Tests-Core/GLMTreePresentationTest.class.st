Class {
	#name : #GLMTreePresentationTest,
	#superclass : #TestCase,
	#instVars : [
		'pane',
		'presentation'
	],
	#category : #'Glamour-Tests-Core'
}

{ #category : #running }
GLMTreePresentationTest >> setUp [
	super setUp.
	pane := GLMPane named: 'test'.
	presentation := GLMTreePresentation new.
	pane addPresentation: presentation.
	presentation entity: #(($a $b $c $d) ($e $f) ($g)).
	presentation children: [:x :i | x asString size > 1 ifTrue: [x] ifFalse: [OrderedCollection new]].
	presentation shouldValidate: true.
]

{ #category : #tests }
GLMTreePresentationTest >> testValidateOn [
	self assert: (presentation validate: #($e $f) on: (pane port: #selection)).
	self assert: (presentation validate: $a on: (pane port: #selection)).
	self deny: (presentation validate: $h on: (pane port: #selection))
]

{ #category : #tests }
GLMTreePresentationTest >> testValidatePorts [
	presentation selection: #($e $f).
	self assert: presentation selection equals: #($e $f).
	presentation selection: $a.
	self assert: presentation selection equals: $a.
	presentation selection: 'wrongSelection'.	"this should be invalid"
	self assert: presentation selection equals: $a
]
