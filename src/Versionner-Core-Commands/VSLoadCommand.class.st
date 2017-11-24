"
Load into your image the selected version or baseline. If you already have a version installed, then it will be simply overriden
"
Class {
	#name : #VSLoadCommand,
	#superclass : #VSVersionCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #execute }
VSLoadCommand >> execute [
	| answer |
	answer := UIManager default 
				confirm: 'Are you sure you want to load ', target name.
	answer ifTrue: 
		[ target version load: target version defaultPackageNamesToLoad ]
]

{ #category : #accessing }
VSLoadCommand >> title [
	^ 'load version'
]
