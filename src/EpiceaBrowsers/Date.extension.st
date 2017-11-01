Extension { #name : #Date }

{ #category : #'*EpiceaBrowsers' }
Date >> epiceaBrowsersAsString [

	| numberOfDaysAgo |
	numberOfDaysAgo := self class today subtractDate: self.

	numberOfDaysAgo < 2 ifTrue: [
		^ String streamContents: [:stream | 
			stream nextPutAll: (numberOfDaysAgo = 0 
				ifTrue: [ 'Today' ]
				ifFalse: [ 'Yesterday'] ) ]].

	(numberOfDaysAgo between: 2 and: 7) ifTrue: [ 
		^ numberOfDaysAgo asString, ' days ago' ].

	^ self asDate asString

]
