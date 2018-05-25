"
I am an object representing the current image to get all sorted packages for browsing.

CurrentImage new packages
	=> all packages in the image sorted
	

THIS CLASS IS NOW DEPRECATED AND OBSOLETE - please use 

  Smalltalk image packages 

to receive all packages in the image sorted
"
Class {
	#name : #CurrentImage,
	#superclass : #Object,
	#category : #Deprecated70
}

{ #category : #testing }
CurrentImage class >> isDeprecated [

	^true
]

{ #category : #accessing }
CurrentImage >> packages [
	self deprecated: 'Use Smalltalk image packages'.
	^ (RPackageOrganizer default packages) sorted
]
