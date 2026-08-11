Extension { #name : 'FTFilterFunction' }

{ #category : '*Calypso-Browser' }
FTFilterFunction >> filterNowWith: patternString [

	self pattern: patternString.
	self field setText: self pattern.
	self filterWith: self pattern.
	self filter
]

{ #category : '*Calypso-Browser' }
FTFilterFunction >> terminateFilterProcess [

	self isEditingSemaphore ifNotNil: [ self isEditingSemaphore terminateProcess ]
]
