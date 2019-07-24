"
i'm an abstractClass
my subclass define :
	- actions : return a Collection of all my subclass sorted by priority
	- priority : return an integer use for set the order of buttons 
	- runOn: aCriticBrowser it's the action of the button
	- title: the label of the button
	
if you want to creat a new button just creat a class inherits from CBAbstractCriticAction or CBAbstractRuleAction and it will appear on your criticBrowser
"
Class {
	#name : #CBAbstractButtonAction,
	#superclass : #Object,
	#category : #'Tool-CriticBrowser-Actions'
}

{ #category : #actions }
CBAbstractButtonAction class >> actions [ 
	^ self subclassResponsibility 
]

{ #category : #accessing }
CBAbstractButtonAction class >> groupPriority [ 
	^ self subclassResponsibility 
]

{ #category : #testing }
CBAbstractButtonAction class >> isAbstract [ 
	^ self = CBAbstractButtonAction 
]

{ #category : #testing }
CBAbstractButtonAction class >> runOn: aCriticBrowser [
	^ self subclassResponsibility 
]

{ #category : #accessing }
CBAbstractButtonAction class >> title [ 
	^ self subclassResponsibility 
]
