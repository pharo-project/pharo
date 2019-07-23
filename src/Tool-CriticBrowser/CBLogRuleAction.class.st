"
see superClass 's documentation
my action is :
	to log all critics
"
Class {
	#name : #CBLogRuleAction,
	#superclass : #CBAbstractRuleAction,
	#category : #'Tool-CriticBrowser-Actions'
}

{ #category : #accessing }
CBLogRuleAction class >> groupPriority [ 
	^ 20
]

{ #category : #testing }
CBLogRuleAction class >> runOn: aCriticBrowser [
	self flag: #pharoFixMe.	"In the long term the UI manager should not be used in Spec application."
	(UIManager default
		confirm:
			'Do you want to save all false positive and toDo in the Manifests ?
(this action may generate new manifest classes and make dirty your package)') ifTrue: [ aCriticBrowser logInManifest ]
]

{ #category : #accessing }
CBLogRuleAction class >> title [ 
	^ 'Save Critics' 
]
