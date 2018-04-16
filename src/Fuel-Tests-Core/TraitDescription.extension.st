Extension { #name : #TraitDescription }

{ #category : #'*Fuel-Tests-Core' }
TraitDescription >> duringTestCompileSilently: code [
	^ self 
		duringTestCompileSilently: code 
		storeSource: true
]

{ #category : #'*Fuel-Tests-Core' }
TraitDescription >> duringTestCompileSilently: code classified: aCategory [
	^ self 
		duringTestCompileSilently: code
		storeSource: true
		classified: aCategory
]

{ #category : #'*Fuel-Tests-Core' }
TraitDescription >> duringTestCompileSilently: code storeSource: storeSource [
	^ self
		duringTestCompileSilently: code
		storeSource: storeSource
		classified: ''
]

{ #category : #'*Fuel-Tests-Core' }
TraitDescription >> duringTestCompileSilently: code storeSource: storeSource classified: aCategory [
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
