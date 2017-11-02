Extension { #name : #ClassDescription }

{ #category : #'*rpackage-core' }
ClassDescription >> compileSilently: code [ 
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: code classified: 'not defined category' notifying: nil.
]

{ #category : #'*rpackage-core' }
ClassDescription >> definedSelectors [
	^ self package definedSelectorsForClass: self
]

{ #category : #'*rpackage-core' }
ClassDescription >> extendingPackages [
	"the extending packages of a class are the packages that extend it."
	^ RPackage organizer extendingPackagesOf: self
]

{ #category : #'*rpackage-core' }
ClassDescription >> extensionSelectors [
	^ self extendingPackages flatCollect: [ :each | each extensionSelectorsForClass: self ]
]

{ #category : #'*rpackage-core' }
ClassDescription >> isDefinedInPackage: aPackage [
	"returns true if aPackage contains the definitino of this class"
	
	^ aPackage includesClass: self.
]

{ #category : #'*rpackage-core' }
ClassDescription >> isExtended [

	^ self extendingPackages isEmpty
]

{ #category : #'*rpackage-core' }
ClassDescription >> isExtendedInPackage: aPackage [
	"returns true if aPackage defines an extension to this class"
	^ aPackage extendsClass: self.
]

{ #category : #'*rpackage-core' }
ClassDescription >> package [
	^ RPackage organizer packageOf: self.	
]

{ #category : #'*rpackage-core' }
ClassDescription >> packageFromOrganizer: anOrganizer [
	"returns the package that defines this class"
	^ anOrganizer packageOf: self.
]

{ #category : #'*rpackage-core' }
ClassDescription >> packageOrganizer [
	"Returns the organizer of this class"
	^ RPackage organizer 
]

{ #category : #'*rpackage-core' }
ClassDescription >> packages [
	"the extending packages of a class are the packages that extend it."
	
	^  self extendingPackages asSet copy add: self package; yourself
]
