"
I collect the time spent by the image builder (see `PBAbstractImageBuilder`) to compile the methods of the new image and to install them.
"
Class {
	#name : 'PBBootstrapStatistics',
	#superclass : 'Object',
	#instVars : [
		'methodTimes',
		'compilationTimes'
	],
	#category : 'PharoBootstrap',
	#package : 'PharoBootstrap'
}

{ #category : 'adding' }
PBBootstrapStatistics >> addCompilationTime: aTime [

	compilationTimes add: aTime.
]

{ #category : 'adding' }
PBBootstrapStatistics >> addMethodTime: aTime [

	methodTimes add: aTime.
]

{ #category : 'initialization' }
PBBootstrapStatistics >> initialize [
	super initialize.
	methodTimes := OrderedCollection new.
	compilationTimes := OrderedCollection new.
]
