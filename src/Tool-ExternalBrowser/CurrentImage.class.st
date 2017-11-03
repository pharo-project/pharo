"
I am an object representing the current image to get all sorted packages for browsing.

CurrentImage new packages
	=> all packages in the image sorted
"
Class {
	#name : #CurrentImage,
	#superclass : #Object,
	#category : #Tool-ExternalBrowser
}

{ #category : #accessing }
CurrentImage >> packages [

	^ (RPackageOrganizer default packages) sorted
]
