"
see superClass 's documentation
my action is :
	to reload the rules
"
Class {
	#name : #CBUpdateRuleAction,
	#superclass : #CBAbstractRuleAction,
	#category : #'Tool-CriticBrowser-Actions'
}

{ #category : #accessing }
CBUpdateRuleAction class >> groupPriority [ 
	^ 10
]

{ #category : #testing }
CBUpdateRuleAction class >> runOn: aCriticBrowser [
	aCriticBrowser reapplyAllRules
]

{ #category : #accessing }
CBUpdateRuleAction class >> title [ 
	^ 'Update'
]
