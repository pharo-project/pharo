Class {
	#name : #FluidBuilder,
	#superclass : #Object,
	#instVars : [
		'shiftClassBuilder',
		'packageName',
		'slotsToBuild',
		'tagToBuild',
		'uses',
		'nameToBuild'
	],
	#category : #'FluidClassBuilder-Base'
}

{ #category : #building }
FluidBuilder >> build [
	"Return a class that is not installed in the system."
	
	^ self shiftClassBuilder build 


]

{ #category : #building }
FluidBuilder >> install [
	"Install the class in the system environment"
	
	^ ShiftClassInstaller new makeWithBuilder: self shiftClassBuilder
]

{ #category : #'accessing - mandatory' }
FluidBuilder >> nameToBuild [
	"mandatory"
	
	^ nameToBuild
]

{ #category : #'accessing - mandatory' }
FluidBuilder >> nameToBuild: aSymbol [ 
	nameToBuild := aSymbol
]

{ #category : #API }
FluidBuilder >> package: aString [

	packageName := aString.

]

{ #category : #'accessing - mandatory' }
FluidBuilder >> packageToBuild [

	^ packageName
]

{ #category : #building }
FluidBuilder >> shiftClassBuilder [ 

	^ shiftClassBuilder ifNil: [ shiftClassBuilder := self fillShiftClassBuilder ]
]

{ #category : #API }
FluidBuilder >> slots: anArray [

	slotsToBuild := anArray
]

{ #category : #accessing }
FluidBuilder >> slotsToBuild [

	^ slotsToBuild
]

{ #category : #accessing }
FluidBuilder >> tag [

	^ tagToBuild
]

{ #category : #API }
FluidBuilder >> tag: aString [

	tagToBuild := aString
]

{ #category : #accessing }
FluidBuilder >> tagToBuild [

	^ tagToBuild
]

{ #category : #API }
FluidBuilder >> trait: anArray [
	"To replace the uses: which will be deprecated"
	uses := anArray
]

{ #category : #accessing }
FluidBuilder >> traitsToBuild [

	^ uses
]
