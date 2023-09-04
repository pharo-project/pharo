Class {
	#name : #UnpackagedPackage,
	#superclass : #AbstractPackage,
	#instVars : [
		'wrapped',
		'rootTag'
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

{ #category : #'class tags' }
UnpackagedPackage >> classTags [

	^ #(  )
]

{ #category : #accessing }
UnpackagedPackage >> classes [

	^ #(  )
]

{ #category : #accessing }
UnpackagedPackage >> definedClasses [

	^ #(  )
]

{ #category : #accessing }
UnpackagedPackage >> definedMethods [

	^ #(  )
]

{ #category : #accessing }
UnpackagedPackage >> definedSelectors [

	^ #(  )
]

{ #category : #accessing }
UnpackagedPackage >> definedSelectorsForClass: aClass [

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
UnpackagedPackage >> extensionMethods [

	^ #(  )
]

{ #category : #accessing }
UnpackagedPackage >> extensionSelectors [

	^ #(  )
]

{ #category : #accessing }
UnpackagedPackage >> extensionSelectorsForClass: aClass [

	^ #(  )
]

{ #category : #initialization }
UnpackagedPackage >> initialize [

	super initialize.
	rootTag := RPackageTag package: self name: self rootTagName
]

{ #category : #testing }
UnpackagedPackage >> isEmpty [

	^ false
]

{ #category : #accessing }
UnpackagedPackage >> name [

	^ self class unpackagedPackageName
]

{ #category : #removing }
UnpackagedPackage >> removeClass: aClass [
	"Do nothing"

	
]

{ #category : #removing }
UnpackagedPackage >> removeEmptyTags [

	
]

{ #category : #removing }
UnpackagedPackage >> removeMethod: aCompiledMethod [
	"Do nothing"

	
]

{ #category : #removing }
UnpackagedPackage >> removeTag: aTag [

	
]

{ #category : #accessing }
UnpackagedPackage >> rootTag [

	^ rootTag
]

{ #category : #'class tags' }
UnpackagedPackage >> tagOf: aClass [

	^ self rootTag
]
