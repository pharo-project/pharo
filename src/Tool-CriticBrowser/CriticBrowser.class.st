"
I display code critics, the results obtained when running a lint rule.

Example: 


| rule env |
rule :=  RBExcessiveArgumentsRule new.
env := (RBPackageEnvironment packageName: 'Manifest-Core').

(CriticBrowser openOnRule: rule onEnvironment: env).
"
Class {
	#name : #CriticBrowser,
	#superclass : #CBCritiqueBrowser,
	#category : #'Tool-CriticBrowser-Base'
}

{ #category : #menu }
CriticBrowser class >> isDeprecated [ 
	"We do not need this class anymore, use CBCritiqueBrowser instead" 

	^true
]
