"
Usage: save <imageBaseName> [--delete-old]
 	<imageName>     a base name for the image
	--delete-old    remove the old image and changes file
	
Documentation:
Saves the image and changes file under a new name.

Examples:
	# create a 'bar.image' and 'foo.changes'
	pharo Pharo.image save bar
	# create the same file as in the previous example but delete Foo.image and Foo.changes
	pharo Pharo.image save bar --delete-old
"
Class {
	#name : #SaveCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : 'System-BasicCommandLineHandler'
}

{ #category : #accessing }
SaveCommandLineHandler class >> commandName [
	^ 'save'
]

{ #category : #accessing }
SaveCommandLineHandler class >> description [
	^ 'Rename the image and changes file'
]

{ #category : #activation }
SaveCommandLineHandler >> activate [
	self activateHelp
		ifTrue: [  ^ self ].
	
	((self hasOption: 'delete-old') 
		ifTrue: [ self arguments size = 2 ]
		ifFalse: [ self arguments size = 1])
			ifFalse: [ 
				self printHelp.
				^ self exitFailure: 'Missing Arguments' ].
		
	self saveImage.
		
]

{ #category : #actions }
SaveCommandLineHandler >> saveImage [
	| imageName oldImageFile oldChangesFile |
	
	imageName := self arguments detect: [ :arg| arg ~= '--delete-old' ].
	oldImageFile := File named: Smalltalk imagePath.
	oldChangesFile := File named: (Smalltalk imagePath copyUpToLast: '.') , '.changes'.
	
	(Smalltalk saveAs: imageName)
		ifTrue: [ 
			"rebooting, when executing this, we are already in the renamed image" 
			^ self ].
	
	"...otherwise continuing"
	(self hasOption: 'delete-old')
		ifFalse: [ ^ self exitSuccess ].
	
	oldImageFile delete.
	oldChangesFile delete.
	
	self exitSuccess.
]
