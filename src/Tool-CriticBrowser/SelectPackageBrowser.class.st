"
To select packages.
"
Class {
	#name : #SelectPackageBrowser,
	#superclass : #CBSelectPackage,
	#category : #'Tool-CriticBrowser-Applier'
}

{ #category : #specs }
SelectPackageBrowser class >> isDeprecated [ 
	"We do not need this class anymore. Use CBSelectPackage instead" 

	^true
]
