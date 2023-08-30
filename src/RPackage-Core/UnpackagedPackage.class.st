Class {
	#name : #UnpackagedPackage,
	#superclass : #AbstractPackage,
	#instVars : [
		'wrapped'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #'instance creation' }
UnpackagedPackage class >> organizer: aPackageOrganizer [

	^ self new
		  organizer: aPackageOrganizer;
		  yourself
]

{ #category : #'reflective operations' }
UnpackagedPackage >> doesNotUnderstand: aMessage [
	('Fowarded ' , aMessage selector) traceCr. 

	^ (wrapped ifNil: [ wrapped := RPackage named: RPackage defaultPackageName organizer: self organizer ])
		  perform: aMessage selector
		  withArguments: aMessage arguments
]
