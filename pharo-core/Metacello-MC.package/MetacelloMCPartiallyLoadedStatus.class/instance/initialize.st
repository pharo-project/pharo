initialization
initialize
	super initialize.
	hasNoPackage := hasNoProject := true.
	aProjectIsLoaded := aPackageIsLoaded := false.
	aLoadedProjectIsExact := aLoadedPackageIsExact := false.
	aLoadedProjectIsCurrent := aLoadedPackageIsCurrent := false.
	aLoadedProjectIsNotCurrent := aLoadedPackageIsNotCurrent := false.
	aProjectNotLoaded := aPackageNotLoaded := false.
	vrsnStatus := Set new.
	abort := false