"
Action when the method is an executable script tagged with the following pragma

myMethodReturningAForm
    <script: 'self myMethodReturningAForm asMorph openInWorld'>
    ...
"
Class {
	#name : #MethodIsScriptWithArgumentAction,
	#superclass : #MethodIsScriptAction,
	#category : #'Nautilus-Widgets'
}

{ #category : #testing }
MethodIsScriptWithArgumentAction >> isActionHandled [
	"Return true if the method is tagged with <script: '...'> pragma for 	      evaluating code"
	
    ^ method pragmas anySatisfy: [:each | each keyword = #script: ]
]

{ #category : #'private - accessing' }
MethodIsScriptWithArgumentAction >> pragmaName [
	"The name of the pragma we handle"
	
	 ^#script:
]

{ #category : #'private - actions' }
MethodIsScriptWithArgumentAction >> runScriptForPragma: pragma [

	self class compiler
			evaluate: pragma arguments first
			for: method methodClass instanceSide
			logged: false

]
