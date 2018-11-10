"
I represent a change coming from the image (in contrast to a change coming from git). I know the changed file and I can be visited to define the create a more detailed diff.
"
Class {
	#name : #IceImageChange,
	#superclass : #Object,
	#instVars : [
		'package'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceImageChange class >> package: anIcePackage [ 
	
	^ self new
		package: anIcePackage;
		yourself
]

{ #category : #building }
IceImageChange >> accept: aVisitor [
	
	^ aVisitor visitImageChange: self
]

{ #category : #displaying }
IceImageChange >> displayingProgressString [
	^ 'Comparing image changes of ' , self package name
]

{ #category : #accessing }
IceImageChange >> package [
	^ package
]

{ #category : #accessing }
IceImageChange >> package: anIcePackage [ 
	package := anIcePackage
]
