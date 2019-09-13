Class {
	#name : #SpImageAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpImageAdapterTest >> classToTest [

	^ SpImagePresenter
]

{ #category : #building }
SpImageAdapterTest >> imageForm [

	^ (PNGReadWriter on: 
			(ZnBase64Encoder new decode: PolymorphSystemSettings pharoLogoContents) readStream) 
			nextImage
]

{ #category : #building }
SpImageAdapterTest >> testAutoscale [

	self presenter image: self imageForm.

	self presenter autoScale: false.
	self deny: self adapter hasImageAutoscaled.

	self presenter autoScale: true.
	self assert: self adapter hasImageAutoscaled.

]

{ #category : #building }
SpImageAdapterTest >> testSettingAnImageSetsTheImage [

	self presenter image: self imageForm.
	backendForTest assertImage: self adapter image equals: self imageForm.

]

{ #category : #building }
SpImageAdapterTest >> testSettingAnImageSetsTheImageAndThrowsEvent [

	| happened |
	self presenter whenImageChangeDo: [ happened := true ].
	self presenter image: self imageForm.
	
	backendForTest assertImage: self adapter image equals: self imageForm.
	self assert: happened
]

{ #category : #building }
SpImageAdapterTest >> testSettingNilClearsTheImage [

	self presenter image: self imageForm.
	self presenter image: nil.
	
	backendForTest assertImage: self adapter image equals: nil.

]
