"
To select rules.
"
Class {
	#name : #SelectRuleBrowser,
	#superclass : #CBSelectRule,
	#category : #'Tool-CriticBrowser-Applier'
}

{ #category : #'instance creation' }
SelectRuleBrowser class >> isDeprecated [ 
	"We do not need this class anymore. Use CBSelectRuleBrowser instead" 

	^true
]
