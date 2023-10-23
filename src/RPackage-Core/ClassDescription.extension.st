Extension { #name : 'ClassDescription' }

{ #category : '*RPackage-Core' }
ClassDescription >> definedSelectors [
	^ self package definedSelectorsForClass: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> extendingPackages [
	"the extending packages of a class are the packages that extend it."

	^ self packageOrganizer extendingPackagesOf: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> extensionSelectors [
	^ self extendingPackages flatCollect: [ :each | each extensionSelectorsForClass: self ]
]

{ #category : '*RPackage-Core' }
ClassDescription >> isDefinedInPackage: aPackage [
	"returns true if aPackage contains the definitino of this class"

	^ aPackage includesClass: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> isExtended [

	^ self extendingPackages isEmpty
]

{ #category : '*RPackage-Core' }
ClassDescription >> isExtendedInPackage: aPackage [
	"returns true if aPackage defines an extension to this class"
	^ aPackage extendsClass: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> removePackageTag [
	"Removing a package tag is the same as moving it to the root tag that is the uncategorized tag."

	| package |
	package := self package.
	package moveClass: self toTag: package rootTag
]
