"
A CriticWorkspace holds the current worspace for the CriticBrowser.

It's composed of the set of rules to check (as a composite rule) and the environment to work in.
"
Class {
	#name : #CriticWorkingConfiguration,
	#superclass : #CBCritiqueWorkingConfiguration,
	#category : #'Tool-CriticBrowser-Utilities'
}

{ #category : #'instance creation' }
CriticWorkingConfiguration class >> isDeprecated [ 
	"We do not need this class anymore. Use CBCritiqueWorkingConfiguration instead" 

	^true
]
