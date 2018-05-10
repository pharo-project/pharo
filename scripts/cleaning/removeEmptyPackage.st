	"This script allow to remove all the packages of the current Pharo repository not present in the image."
	
	| pharo sources allPackages |
	"/!\ Change the value of this line by the path to your local Pharo clone"
	pharo := 'pharo' asFileReference.
	
	allPackages := RPackageOrganizer default packages.
	sources := pharo / 'src'.
	sources directories
		select: [ :each | 
			| packageFile |
			packageFile := each / 'package.st'.
			packageFile exists
				ifFalse: [ false ]
				ifTrue: [ | packageName |
					packageName := each basename.
					allPackages detect: [ :package | package name = packageName ] ifFound: [ false ] ifNone: [ true ] ] ]
		thenDo: [ :each | each ensureDeleteAll ]