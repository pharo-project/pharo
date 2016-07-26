private
addHacks
	"Simply load FuelPlatform. The class side initialization will take care of installing
	extensions and hacks"
	(Smalltalk hasClassNamed: #FLPlatform) and: [
		((Smalltalk at: #FLPlatform) class canUnderstand: #checkForUpdates) ifTrue: [
		(Smalltalk at: #FLPlatform) checkForUpdates.
		^ self ] ].
	
	Gofer it
		package: 'FuelPlatform';
		url: 'http://smalltalkhub.com/mc/Pharo/Fuel/main';
		load