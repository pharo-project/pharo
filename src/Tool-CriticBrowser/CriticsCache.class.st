"
I am a cache for the critics and false positives critics

Instance Variables
	browser:		<Object>
	checker:		<Object>
	critics:		<Object>
	falsePositiveClasses:		<Object>
	falsePositiveRules:		<Object>
	falsePositives:		<Object>
	packages:		<Object>
	toDos:		<Object>

browser
	- xxxxx

checker
	- xxxxx

critics
	- xxxxx

falsePositiveClasses
	- xxxxx

falsePositiveRules
	- xxxxx

falsePositives
	- xxxxx

packages
	- xxxxx

toDos
	- xxxxx

"
Class {
	#name : #CriticsCache,
	#superclass : #CBCritiquesCache,
	#category : #'Tool-CriticBrowser-Utilities'
}

{ #category : #testing }
CriticsCache class >> isDeprecated [ 
	"We do not need this class anymore. Use CBCritiquesCache instead" 

	^true
]
