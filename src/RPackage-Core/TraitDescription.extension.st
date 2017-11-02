Extension { #name : #TraitDescription }

{ #category : #'*rpackage-core' }
TraitDescription >> compileSilently: code [ 
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: code classified: 'not defined category' notifying: nil.
]

{ #category : #'*rpackage-core' }
TraitDescription >> definedSelectors [
	^ self package definedSelectorsForClass: self
]

{ #category : #'*rpackage-core' }
TraitDescription >> extendingPackages [
	"the extending packages of a class are the packages that extend it."
	^ RPackage organizer extendingPackagesOf: self
]

{ #category : #'*rpackage-core' }
TraitDescription >> extensionSelectors [
	^ self extendingPackages flatCollect: [ :each | each extensionSelectorsForClass: self ]
]

{ #category : #'*rpackage-core' }
TraitDescription >> isDefinedInPackage: aPackage [
	"returns true if aPackage contains the definitino of this class"
	
	^ aPackage includesClass: self.
]

{ #category : #'*rpackage-core' }
TraitDescription >> isExtended [

	^ self extendingPackages isEmpty
]

{ #category : #'*rpackage-core' }
TraitDescription >> isExtendedInPackage: aPackage [
	"returns true if aPackage defines an extension to this class"
	^ aPackage extendsClass: self.
]

{ #category : #'*rpackage-core' }
TraitDescription >> package [
	^ RPackage organizer packageOf: self.	
]

{ #category : #'*rpackage-core' }
TraitDescription >> packageFromOrganizer: anOrganizer [
	"returns the package that defines this class"
	^ anOrganizer packageOf: self.
]

{ #category : #'*rpackage-core' }
TraitDescription >> packageOrganizer [
	"Returns the organizer of this class"
	^ RPackage organizer 
]

{ #category : #'*rpackage-core' }
TraitDescription >> packages [
	"the extending packages of a class are the packages that extend it."
	
	^  self extendingPackages asSet copy add: self package; yourself
]
