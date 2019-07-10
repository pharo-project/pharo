Extension { #name : #SycRemoveMethodStrategy }

{ #category : #'*Calypso-SystemTools-Core' }
SycRemoveMethodStrategy class >> createAllForBrowser: aBrowser [

	| result |
	result := SortedCollection sortBlock: #orderForBrowser ascending.
	
	self allSubclassesDo: [ :each | 
		each hasSubclasses ifFalse: [ 
			result add: (each newForBrowser: aBrowser) ]].
	
	^result
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRemoveMethodStrategy class >> newForBrowser: aBrowser [

	^self new
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRemoveMethodStrategy >> orderForBrowser [
	"by default any new strategy will be at the end"
	^1000
]
