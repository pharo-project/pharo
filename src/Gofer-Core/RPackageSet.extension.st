Extension { #name : #RPackageSet }

{ #category : #'*Gofer-Core' }
RPackageSet >> classesAndMetaClasses [
	^ self packages
		flatCollect: [ :p | p definedClasses , (p definedClasses collect: [ :each | each classSide ]) ]
]

{ #category : #'*Gofer-Core' }
RPackageSet >> coreCategoriesForClass: aClass [
	^ self packages
		flatCollect: [ :p | aClass organization categories reject: [ :cat | p isForeignClassExtension: cat ] ]
]
