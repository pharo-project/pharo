"
I am a project whose aim is to contain all the packages that are in no project in the environment.
"
Class {
	#name : 'UndefinedProject',
	#superclass : 'Project',
	#instVars : [
		'packages'
	],
	#category : 'Tools',
	#package : 'Tools'
}

{ #category : 'instance creation' }
UndefinedProject class >> packages: aCollection [

	^ self new
		  packages: aCollection;
		  yourself
]

{ #category : 'accessing' }
UndefinedProject >> name [

	^ #'Undefined Project'
]

{ #category : 'accessing' }
UndefinedProject >> packages [

	^ packages
]

{ #category : 'accessing' }
UndefinedProject >> packages: aCollection [

	packages := aCollection
]
