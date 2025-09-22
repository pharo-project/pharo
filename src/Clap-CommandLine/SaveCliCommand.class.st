"
Usage: save <imageBaseName> [--delete-old]
 	<imageName>     a base name for the image
	--delete-old    remove the old image and changes file
	
Documentation:
Saves the image and changes file under a new name.

Examples:
	# create a 'foo.image' and 'foo.changes'
	pharo Pharo.image save foo
	# create the same files as in the previous example but delete Foo.image and Foo.changes
	pharo Pharo.image save bar --delete-old
"
Class {
	#name : 'SaveCliCommand',
	#superclass : 'ClapPharoApplication',
	#category : 'Clap-CommandLine',
	#package : 'Clap-CommandLine'
}

{ #category : 'command line' }
SaveCliCommand class >> commandSpecification [

	<commandline>
	^ (ClapCommandSpec id: #'save-command')
	     canonicalName: 'save';
		  description: 'Saves the image and changes file under a new name.';
		  commandClass: self;
		  addHelp;
		  addFlag: #'delete-old' description: 'remove the old image and changes file';
		  addPositional: #imageName description: 'the new name (without extension) of the image to be saved';
		  yourself
]

{ #category : 'execution' }
SaveCliCommand >> execute [

	| oldImageFile oldChangesFile |
	self isValidImageName ifFalse: [ ^ self exitFailure: 'Image name is mandatory' ].
	
	oldImageFile := File named: Smalltalk imagePath.
	oldChangesFile := File named: (Smalltalk imagePath copyUpToLast: $.) , '.changes'.
	
	(Smalltalk saveAs: self imageName) isImageStarting 
		ifTrue: [ "rebooting, when executing this, we are already in the renamed image" ^ self ].
	
	"...otherwise continuing"
	self shouldDeleteOldImage ifFalse: [ ^ self ].
	oldImageFile delete.
	oldChangesFile delete
]

{ #category : 'accessing' }
SaveCliCommand >> imageName [

	^ self positional: #imageName
]

{ #category : 'testing' }
SaveCliCommand >> isValidImageName [

	^ [ self imageName isNotNil and: [ self imageName isNotEmpty ] ]
	on: NotFound
	do: [ false ]
]

{ #category : 'testing' }
SaveCliCommand >> shouldDeleteOldImage [

	^ self hasFlag: #'delete-old'
]
