Extension { #name : #FTFilterFunction }

{ #category : #'*Calypso-Browser' }
FTFilterFunction >> field [
	^field
]

{ #category : #'*Calypso-Browser' }
FTFilterFunction >> filterNowWith: patternString [
	pattern := patternString.
	self field setText: pattern.
	self filterWith: pattern.
	self filter.
	
]

{ #category : #'*Calypso-Browser' }
FTFilterFunction >> initialDataSource [
	^initialDataSource
]

{ #category : #'*Calypso-Browser' }
FTFilterFunction >> initialDataSource: aDataSource [
	initialDataSource := aDataSource
]

{ #category : #'*Calypso-Browser' }
FTFilterFunction >> pattern [
	^pattern
]

{ #category : #'*Calypso-Browser' }
FTFilterFunction >> terminateFilterProcess [
	isEditingSemaphore ifNotNil: [ isEditingSemaphore terminateProcess ]
]
