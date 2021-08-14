"
I am the toolbar for a selected SmallLint Critic in the critic browser.


Instance Variables
	browseModel:		<Object>
	criticHolder:		<Object>
	falsepositiveModel:		<Object>
	ruleHolder:		<Object>
	transformModel:		<Object>

browseModel
	- xxxxx

criticHolder
	- xxxxx

falsepositiveModel
	- xxxxx

ruleHolder
	- xxxxx

transformModel
	- xxxxx

"
Class {
	#name : #CriticToolbar,
	#superclass : #CBCritiqueToolbar,
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
CriticToolbar class >> isDeprecated [ 
	"We do not need this class anymore. Use CBCritiqueToolbar instead" 

	^true
]
