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

{ #category : #unpackaged }
UnpackagedPackage class >> unpackagedPackageName [

	^ #_UnpackagedPackage
]

{ #category : #accessing }
UnpackagedPackage >> definedClasses [

	^ #(  )
]

{ #category : #'reflective operations' }
UnpackagedPackage >> doesNotUnderstand: aMessage [
	('Fowarded ' , aMessage selector) traceCr. 

	^ (wrapped ifNil: [ wrapped := RPackage named: RPackage defaultPackageName organizer: self organizer ])
		  perform: aMessage selector
		  withArguments: aMessage arguments
]

{ #category : #accessing }
UnpackagedPackage >> extendedClasses [

	^ #(  )
]

{ #category : #accessing }
UnpackagedPackage >> name [

	^ self class unpackagedPackageName
]
