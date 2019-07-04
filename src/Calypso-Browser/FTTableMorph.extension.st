Extension { #name : #FTTableMorph }

{ #category : #'*Calypso-Browser' }
FTTableMorph >> activateFilterWith: patternString [
	(function isKindOf: FTFilterFunction) ifFalse: [ ^self ].
	
	function filterNowWith: patternString
]

{ #category : #'*Calypso-Browser' }
FTTableMorph >> cleanupFilter [
	"it is ugly. but now it is easy to do like this. Normally it should be somehow supported by table package itself"
	(function isKindOf: FTFilterFunction)
		ifTrue: [ function terminateFilterProcess]
]

{ #category : #'*Calypso-Browser' }
FTTableMorph >> filterField [
	^(function isKindOf: FTFilterFunction )
		ifTrue: [ function field]
		ifFalse: [ nil ]
]

{ #category : #'*Calypso-Browser' }
FTTableMorph >> filterString [
	(function isKindOf: FTFilterFunction) ifFalse: [ ^'' ]. 
	^function pattern ifNil: [ '' ]
]

{ #category : #'*Calypso-Browser' }
FTTableMorph >> initialDataSource [
	"it is ugly. but now it is easy to do like this. Normally it should be somehow supported by table package itself"
	^(function isKindOf: FTFilterFunction)
		ifTrue: [ function initialDataSource]
		ifFalse: [ nil ]
]

{ #category : #'*Calypso-Browser' }
FTTableMorph >> initialDataSource: aDataSource [
	"it is ugly. but now it is easy to do like this. Normally it should be somehow supported by table package itself"
	(function isKindOf: FTFilterFunction)
		ifTrue: [ function initialDataSource: aDataSource]
]
