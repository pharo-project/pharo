"
A NoBrowser is a browser without behavior and that does not require a container pane. The root pane is always placed in a NoBrowser.
Given that it has no behavior, a NoBrowser is a singleton to avoid multiple unnecessary instances.
"
Class {
	#name : #GLMNoBrowser,
	#superclass : #GLMLoggedObject,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'Glamour-Core'
}

{ #category : #'class initialization' }
GLMNoBrowser class >> initialize [

	self initializeUniqueInstance
]

{ #category : #'class initialization' }
GLMNoBrowser class >> initializeUniqueInstance [

	uniqueInstance := self basicNew initialize
]

{ #category : #'instance creation' }
GLMNoBrowser class >> new [

	^self uniqueInstance
]

{ #category : #accessing }
GLMNoBrowser class >> uniqueInstance [

	^uniqueInstance
]

{ #category : #accessing }
GLMNoBrowser >> innerPortEvent: aPortEvent [ 
	"Needed for simulating a browser"
]

{ #category : #accessing }
GLMNoBrowser >> lastActivePresentation: aPresentation [
]
