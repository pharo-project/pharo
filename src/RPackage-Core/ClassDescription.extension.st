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
ClassDescription >> package [

	^ self packageOrganizer packageOf: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> package: aPackage [

	^ (self packageOrganizer ensurePackage: aPackage) addClass: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> package: aPackage tag: aTag [

	^ (self packageOrganizer ensureTag: aTag inPackage: aPackage) addClass: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> packageFromOrganizer: anOrganizer [
	"returns the package that defines this class"
	^ anOrganizer packageOf: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> packageName [

	^ self package ifNotNil: [ :package | package name ]
]

{ #category : '*RPackage-Core' }
ClassDescription >> packageTag [
	"Package tags are sub categories of packages to have a better organization of the packages."

	^ self package tagOf: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> packageTag: aTag [
	(aTag isString ifTrue: [ self package ensureTag: aTag ] ifFalse: [ aTag ]) addClass: self
]

{ #category : '*RPackage-Core' }
ClassDescription >> packageTagName [
	"Package tags are sub categories of packages to have a better organization of the packages. I return the name of my package tag or nil if the class is uncategorized."

	^ self packageTag ifNotNil: [ :tag | tag name ]
]

{ #category : '*RPackage-Core' }
ClassDescription >> packages [
	"the extending packages of a class are the packages that extend it."

	^  self extendingPackages asSet copy add: self package; yourself
]

{ #category : '*RPackage-Core' }
ClassDescription >> removePackageTag [
	"Removing a package tag is the same as moving it to the root tag that is the uncategorized tag."

	| package |
	package := self package.
	package moveClass: self toTag: package rootTag
]
