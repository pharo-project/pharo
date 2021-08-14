"
I am a browser for the SmallLint Critics of a rule.
"
Class {
	#name : #SingleCodeCriticResultList,
	#superclass : #CBSingleCodeCritiqueResultList,
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #icons }
SingleCodeCriticResultList class >> isDeprecated [ 
	"We do not need this class anymore. Use CBSingleCodeCriticResultList instead" 

	^true
]
