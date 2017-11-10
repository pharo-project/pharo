"
I am used to temporarily store a version for a package while a cherry-picked slice is committed
"
Class {
	#name : #KomitterManager,
	#superclass : #Object,
	#instVars : [
		'versions'
	],
	#classInstVars : [
		'instance'
	],
	#category : #Komitter-Support
}

{ #category : #accessing }
KomitterManager class >> current [

	^ instance ifNil: [ instance := super new ]
]

{ #category : #'instance creation' }
KomitterManager class >> new [

	^ self shouldNotImplement
]

{ #category : #initialize }
KomitterManager >> initialize [

	super initialize.

	versions := Dictionary new.
]

{ #category : #protocol }
KomitterManager >> reset [

	versions := Dictionary new
]

{ #category : #protocol }
KomitterManager >> storeVersion: aVersion for: aPackage [

	versions 
		at: aPackage
		put: aVersion
]

{ #category : #protocol }
KomitterManager >> versionFor: aPackage [

	^ versions 
		at: aPackage
]
