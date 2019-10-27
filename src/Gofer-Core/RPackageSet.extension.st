Extension { #name : #RPackageSet }

{ #category : #'*Gofer-Core' }
RPackageSet >> classesAndMetaClasses [
	^ self
		collectFromAllPackages: [ :p | 
			OrderedCollection new
				addAll: p definedClasses;
				addAll: (p definedClasses collect: [ :each | each classSide ]);
				yourself ]
]

{ #category : #'*Gofer-Core' }
RPackageSet >> coreCategoriesForClass: aClass [
	^ self
		collectFromAllPackages: [ :p | p organization categories reject: [ :cat | p isForeignClassExtension: cat ] ]
		with: aClass
]
