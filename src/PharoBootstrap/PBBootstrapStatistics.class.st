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

{ #category : 'adding' }
PBBootstrapStatistics >> initialize [
	super initialize.
	methodTimes := OrderedCollection new.
	compilationTimes := OrderedCollection new.
]
