Class {
	#name : #GLMListPresentationTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMListPresentationTest >> testChildren [
	| tree |
	tree := GLMTreePresentation new.
	tree transformation: [:aNumber | (1 to: aNumber ) collect: #printString ].
	tree children: [:each | self assert: (each isKindOf: String)  ].
	tree entity: 42.
]

{ #category : #tests }
GLMListPresentationTest >> testCustomValidation [
	| pane presentation |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	presentation customValidation: [ :object :aport :entity | aport ~= #selection or: [ object = (entity last + 40) ] ].
	presentation entity: (OrderedCollection with: 1 with: 2).
	self assert: presentation entity last equals: 2.
	self assert: (presentation validate: 42 on: (pane port: #selection))
]

{ #category : #tests }
GLMListPresentationTest >> testSorted [
	| presentation | 
	presentation := GLMListPresentation new. 
	presentation entity: #(#foo #bar #blurp).
	self assert: presentation displayValue equals: #(#foo #bar #blurp).
	presentation sorted. 
	self assert: presentation displayValue equals: #(#bar #blurp #foo).
	presentation sorted: [:a :b | a size < b size].  
	self assert: presentation displayValue equals: #(#bar #foo #blurp).
]

{ #category : #tests }
GLMListPresentationTest >> testToSend [
	| list |
	list := GLMListPresentation new.
	list selectionTransformation: [ :x | x + 1 ].
	list entity: (1 to: 100).
	list selection: 41.
	self assert: list selection equals: 42
]

{ #category : #tests }
GLMListPresentationTest >> testValidateOn [
	| pane presentation |
	pane := GLMPane named: 'test'. 
	presentation := GLMListPresentation new.
	pane addPresentation: presentation.
	presentation shouldValidate: true.
	presentation entity: ($a to: $d).
	self deny: (presentation validate: (Array with: $a) on: (pane port: #selection)).
	self deny: (presentation validate: (Array with: $a with: $b) on: (pane port: #selection)).
	self assert: (presentation validate: $a on: (pane port: #selection)).
	self assert: (presentation validate: $b on: (pane port: #selection)).
	self deny: (presentation validate: $z on: (pane port: #selection)).
]

{ #category : #tests }
GLMListPresentationTest >> testValidatePorts [
	| pane presentation |
	pane := GLMPane named: 'test'.
	presentation := GLMListPresentation new.
	pane addPresentation: presentation.
	presentation shouldValidate: true.
	presentation entity: #(1 2).
	presentation selection: 2.
	self assert: presentation selection equals: 2.
	presentation selection: 3.	"this should be invalid"
	self assert: presentation selection equals: 2
]

{ #category : #tests }
GLMListPresentationTest >> testValidateWithMultipleSelection [
	| pane presentation |
	pane := GLMPane named: 'test'. 
	presentation := GLMListPresentation new.
	presentation beMultiple.
	presentation shouldValidate: true.
	pane addPresentation: presentation.
	presentation entity: ($a to: $d).
	self assert: (presentation validate: (Array with: $a) on: (pane port: #selection)).
	self assert: (presentation validate: (Array with: $a with: $b) on: (pane port: #selection)).
	self deny: (presentation validate: ($a) on: (pane port: #selection)).
	self deny: (presentation validate: (Array with: $z) on: (pane port: #selection)).
]
