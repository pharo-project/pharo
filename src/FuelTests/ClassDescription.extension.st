Extension { #name : #ClassDescription }

{ #category : #'*FuelTests' }
ClassDescription >> duringTestCompileSilently: code [
	^ self 
		duringTestCompileSilently: code 
		storeSource: true
]

{ #category : #'*FuelTests' }
ClassDescription >> duringTestCompileSilently: code classified: aCategory [
	^ self 
		duringTestCompileSilently: code
		storeSource: true
		classified: aCategory
]

{ #category : #'*FuelTests' }
ClassDescription >> duringTestCompileSilently: code storeSource: storeSource [
	^ self
		duringTestCompileSilently: code
		storeSource: storeSource
		classified: ''
]

{ #category : #'*FuelTests' }
ClassDescription >> duringTestCompileSilently: code storeSource: storeSource classified: aCategory [
	^ FLPlatform current 
		useAuthor: 'TestsAuthor'
		during: [ [
			self 
				compile: code
				classified: (aCategory ifNil: [ '' ])
				withStamp: nil 
				notifying: nil 
				logSource: storeSource ] fuelValueWithoutNotifications ]
]
