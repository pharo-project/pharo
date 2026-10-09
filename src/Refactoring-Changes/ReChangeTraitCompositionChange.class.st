"
I change a behavior's trait composition after its referenced traits exist.
The expression is resolved in the execution environment, so a package copy can
refer to traits created by preceding changes. Undo restores the old expression.
"
Class {
	#name : 'ReChangeTraitCompositionChange',
	#superclass : 'ReClassChange',
	#instVars : [ 'traitCompositionExpression' ],
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReChangeTraitCompositionChange class >> change: aClassName to: anExpression [

	^ self new
		changeClassName: aClassName;
		traitCompositionExpression: anExpression;
		yourself
]

{ #category : 'converting' }
ReChangeTraitCompositionChange >> asUndoOperation [

	^ (self class
		change: self changeClassName
		to: self changeClass traitComposition traitCompositionExpression)
		onSystemEnvironment: onSystemEnvironment;
		yourself
]

{ #category : 'printing' }
ReChangeTraitCompositionChange >> changeString [

	^ self displayClassName , ' uses: ' , traitCompositionExpression
]

{ #category : 'private' }
ReChangeTraitCompositionChange >> generateChanges [

	| composition |
	composition := (Smalltalk compiler
		environment: onSystemEnvironment;
		source: traitCompositionExpression;
		isScripting: true;
		compileOrFail) valueWithReceiver: nil.
	self class classInstaller make: [ :builder |
		builder
			fillFor: self changeClass;
			traitComposition: composition ]
]

{ #category : 'accessing' }
ReChangeTraitCompositionChange >> traitCompositionExpression: anExpression [

	traitCompositionExpression := anExpression
]
