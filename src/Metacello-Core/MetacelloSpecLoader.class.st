Class {
	#name : #MetacelloSpecLoader,
	#superclass : #Object,
	#instVars : [
		'spec'
	],
	#category : #'Metacello-Core-Loaders'
}

{ #category : #'instance creation' }
MetacelloSpecLoader class >> on: aMetacelloPackagesSpec [

	^(self new) 
		spec: aMetacelloPackagesSpec;
		yourself
]

{ #category : #actions }
MetacelloSpecLoader >> load [

	self subclassResponsibility
]

{ #category : #accessing }
MetacelloSpecLoader >> loadType [
	"#atomic or #linear"
	
	^self project loadType
]

{ #category : #accessing }
MetacelloSpecLoader >> project [

	^self spec project
]

{ #category : #accessing }
MetacelloSpecLoader >> spec [

	^spec
]

{ #category : #accessing }
MetacelloSpecLoader >> spec: aMetacelloPackagesSpec [

	spec := aMetacelloPackagesSpec
]

{ #category : #actions }
MetacelloSpecLoader >> unload [

	self subclassResponsibility
]
