Extension { #name : #BaselineOf }

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> allPackageNames [
	
	^ self version packages collect: #name
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> packagesOfGroupNamed: aName [

	^ (self version groups detect: [ :g | g name = aName ]) includes
]

{ #category : #'*Metacello-PharoExtensions' }
BaselineOf class >> version [
	
	^ self project version
]
