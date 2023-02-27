Extension { #name : #ClassDescription }

{ #category : #'*RPackage-Core' }
ClassDescription >> compileSilently: code [
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: code classified: 'not defined category' notifying: nil
]

{ #category : #'*RPackage-Core' }
ClassDescription >> definedSelectors [
	^ self package definedSelectorsForClass: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> extendingPackages [
	"the extending packages of a class are the packages that extend it."
	^ RPackage organizer extendingPackagesOf: self
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
	^ RPackage organizer packageOf: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> packageFromOrganizer: anOrganizer [
	"returns the package that defines this class"
	^ anOrganizer packageOf: self
]

{ #category : #'*RPackage-Core' }
ClassDescription >> packageOrganizer [
	"Returns the organizer of this class"
	^ RPackage organizer
]

{ #category : #'*RPackage-Core' }
ClassDescription >> packages [
	"the extending packages of a class are the packages that extend it."

	^  self extendingPackages asSet copy add: self package; yourself
]
