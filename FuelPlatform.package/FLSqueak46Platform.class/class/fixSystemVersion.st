private-hacks
fixSystemVersion
	"This is ridiculous but until release the 4.6 image seems to identify itself as 4.5 (WTF!)"
	(Smalltalk imageName includesSubString: '4.6') ifFalse: [ ^ self ].

	SystemVersion current version: 'Squeak4.6'