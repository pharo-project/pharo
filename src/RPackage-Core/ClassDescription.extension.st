Extension { #name : #ClassDescription }

{ #category : #'*RPackage-Core' }
ClassDescription >> definedSelectors [
	^ self package definedSelectorsForClass: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> extendingPackages [
	"the extending packages of a class are the packages that extend it."
	^ self packageOrganizer extendingPackagesOf: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> extensionSelectors [
	^ self extendingPackages flatCollect: [ :each | each extensionSelectorsForClass: self ]
]

{ #category : #'*RPackage-Core' }
ClassDescription >> isDefinedInPackage: aPackage [
	"returns true if aPackage contains the definitino of this class"

	^ aPackage includesClass: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> isExtended [

	^ self extendingPackages isEmpty
]

{ #category : #'*RPackage-Core' }
ClassDescription >> isExtendedInPackage: aPackage [
	"returns true if aPackage defines an extension to this class"
	^ aPackage extendsClass: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> package [

	^ self packageOrganizer packageOf: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> packageFromOrganizer: anOrganizer [
	"returns the package that defines this class"
	^ anOrganizer packageOf: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> packageTag [

	| packageTag |
	"Any class or trait could be tagged by a symbol for user purpose.
	For now we only define API to manage them on top of RPackageTag"
	packageTag := self package classTagForClass: self.
	packageTag ifNil: [ ^ nil ].
	packageTag isRoot ifTrue: [ ^ nil ].

	^ packageTag name
]

{ #category : #'*RPackage-Core' }
ClassDescription >> packages [
	"the extending packages of a class are the packages that extend it."

	^  self extendingPackages asSet copy add: self package; yourself
]
