accessing
addFontInstaller: aFontInstaller
	(fontInstallers includes: aFontInstaller) ifTrue: [ ^ self ].
	fontInstallers add: aFontInstaller