accessing
packageAt: aPackageName
	"answer a PDPackage"
	(packages keys includes: aPackageName) ifTrue: [  
	^ packages at: aPackageName ].
	^ nil