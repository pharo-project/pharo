Class {
	#name : #ClapPharoApplication,
	#superclass : #ClapApplication,
	#category : #'Clap-Commands-Pharo'
}

{ #category : #execution }
ClapPharoApplication >> execute [
	arguments validateAll.
	arguments at: #helpFlag ifPresent: [ :help | help value; exitSuccess ].

	self prepareImage
]

{ #category : #execution }
ClapPharoApplication >> prepareImage [
	arguments at: #rename ifPresent: [ :rename | 
		| imageFile |
		imageFile := rename value asFileReference.
		self flag: 'check if it already exists & not same as current'.
		Smalltalk image
				changeImagePathTo: imageFile;
				closeSourceFiles;
				openSourceFiles ]
]

{ #category : #execution }
ClapPharoApplication >> shouldSave [
	^ (arguments at: #save) isExplicit
		or: [ (arguments at: #noSave) isExplicit not ]
]
