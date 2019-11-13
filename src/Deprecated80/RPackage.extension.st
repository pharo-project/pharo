Extension { #name : #RPackage }

{ #category : #'*Deprecated80' }
RPackage >> categoryName [
	self
		deprecated: 'This method will be removed. Clients should call name directly'
		transformWith: '`@receiver categoryName' -> '`@receiver name'.
	^ self name
]

{ #category : #'*Deprecated80' }
RPackage >> classesAndMetaClasses [
	self deprecated: 'This method will be removed from Pharo 9 to make the API lighter.' on: '26-10-2019' in: #Pharo8.
	^ OrderedCollection new
		addAll: self definedClasses;
		addAll: (self definedClasses collect: [ :each | each classSide ]);
		yourself
]

{ #category : #'*Deprecated80' }
RPackage >> coreCategoriesForClass: aClass [
	self deprecated: 'This method will be removed from Pharo 9 to make the API lighter.' on: '26-10-2019' in: #Pharo8.
	^ aClass organization categories 
		reject: [ :cat | self isForeignClassExtension: cat ]
]

{ #category : #'*Deprecated80' }
RPackage >> extensionClasses [
	self deprecated: 'Use #extensionClasses instead.' transformWith: '`@receiver extensionClasses' -> '`@receiver extendedClasses'.
	^ self extendedClasses
]
