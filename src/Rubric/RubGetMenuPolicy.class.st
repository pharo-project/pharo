"
I am an abstract class to get the correct menu of a text area
"
Class {
	#name : #RubGetMenuPolicy,
	#superclass : #Object,
	#instVars : [
		'textArea'
	],
	#category : #Rubric-Editing-Core
}

{ #category : #'accessing menu' }
RubGetMenuPolicy >> lookupMenu [ 
	^ textArea lookupMenu
]

{ #category : #'accessing menu' }
RubGetMenuPolicy >> lookupShiftMenu [ 
	^ textArea lookupShiftMenu
]

{ #category : #accessing }
RubGetMenuPolicy >> textArea [
	^ textArea
]

{ #category : #accessing }
RubGetMenuPolicy >> textArea: aTextArea [
	textArea := aTextArea
]
