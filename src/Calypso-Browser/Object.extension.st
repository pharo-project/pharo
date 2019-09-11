Extension { #name : #Object }

{ #category : #'*Calypso-Browser' }
Object class >> asCalypsoItemContext [
	^ClyBrowserItemContext itemType: self
]

{ #category : #'*Calypso-Browser' }
Object >> asScriptResultStringInCalypso [
	^self printString
]
