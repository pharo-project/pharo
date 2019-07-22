"
i'm an Abstract class to define feature of a button:

i define:

	- actions : return a collection of all my subclass sorted by priority method

my subclass define:
	
	- priority : return an integer use for set the order of buttons 
	- runOn: aCriticBrowser it's the action of the button
	- title: the label of the button
"
Class {
	#name : #CBAbstractRuleAction,
	#superclass : #CBAbstractButtonAction,
	#category : #'Tool-CriticBrowser-utilities-buttons'
}

{ #category : #actions }
CBAbstractRuleAction class >> actions [
	^ self allSubclasses sorted: #groupPriority ascending 
]

{ #category : #testing }
CBAbstractRuleAction class >> isAbstract [
	^ self = CBAbstractRuleAction
]
