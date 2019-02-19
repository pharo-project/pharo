Extension { #name : #ClassDescription }

{ #category : #'*Fuel-Tests-Core' }
ClassDescription >> duringTestCompileSilently: code [
	^ self 
		duringTestCompileSilently: code 
		storeSource: true
]

{ #category : #'*Fuel-Tests-Core' }
ClassDescription >> duringTestCompileSilently: code classified: aCategory [
	^ self 
		duringTestCompileSilently: code
		storeSource: true
		classified: aCategory
]

{ #category : #'*Fuel-Tests-Core' }
ClassDescription >> duringTestCompileSilently: code storeSource: storeSource [
	^ self
		duringTestCompileSilently: code
		storeSource: storeSource
		classified: #Unclassified
]

{ #category : #'*Fuel-Tests-Core' }
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
