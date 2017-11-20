"
A base superclass for rules that check AST nodes.
"
Class {
	#name : #ReNodeBasedRule,
	#superclass : #ReAbstractRule,
	#category : #Renraku-Rules-Rewrite
}

{ #category : #testing }
ReNodeBasedRule class >> checksNode [

	^ true
]

{ #category : #testing }
ReNodeBasedRule class >> isVisible [

	^ self name ~=  #ReNodeBasedRule
]

{ #category : #accessing }
ReNodeBasedRule class >> uniqueIdentifierName [

	^ self name
]

{ #category : #helpers }
ReNodeBasedRule >> anchorFor: aNode [

	^ ReIntervalSourceAnchor
			entity: aNode
			interval: aNode sourceInterval.
			
	
]
