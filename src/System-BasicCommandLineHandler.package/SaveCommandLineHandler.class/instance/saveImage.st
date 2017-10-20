actions
saveImage
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