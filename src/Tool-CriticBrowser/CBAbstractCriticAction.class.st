"
this class is an Abstract Class to defined button interact with Item in CriticsPresenter

	see documentation of my SuperClass 
	
i define :
	- actions : return a Collection of all my subclass sorted by priority
	
my subclass define:
	- priority : return an integer use for set the order of buttons 
	- runOn: aCriticBrowser it's the action of the button
	- title: the label of the button 
"
Class {
	#name : #CBAbstractCriticAction,
	#superclass : #CBAbstractButtonAction,
	#category : #'Tool-CriticBrowser-utilities-buttons'
}

{ #category : #actions }
CBAbstractCriticAction class >> actions [
	^ self allSubclasses sorted: #groupPriority ascending 
]

{ #category : #accessing }
CBAbstractCriticAction class >> groupPriority [
	^ self subclassResponsibility 
]

{ #category : #testing }
CBAbstractCriticAction class >> isAbstract [
	^ self = CBAbstractCriticAction
]

{ #category : #accessing }
CBAbstractCriticAction class >> runOn: aCriticBrowser [
	^ self subclassResponsibility
]
