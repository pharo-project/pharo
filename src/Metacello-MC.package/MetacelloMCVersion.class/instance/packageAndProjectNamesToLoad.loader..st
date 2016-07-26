querying
packageAndProjectNamesToLoad: defaultList loader: aLoader
	"Answer the list of packages and projects to be loaded: packages already loaded plust defaultList"
	
	| loadedPackageNames projectMap loadedProjectNames list |
	loadedPackageNames := ((self packages select: [:pkg | pkg isPackageLoaded: aLoader ]) 
		collect: [:pkg | pkg name ]) asSet, 
			defaultList.
	projectMap := Dictionary new.
	self  projects do: [:prj | 
		prj className ~~ nil
			ifTrue: [ | coll loaded |
				coll := projectMap at: prj className ifAbsent: [
					coll := OrderedCollection new.
					projectMap at: prj className put: coll.
					coll].
				(loaded := prj loadedPackageNames: aLoader) isEmpty 
					ifFalse: [  coll add: prj -> (loaded -> prj loadPackageList) ]]].
	loadedProjectNames := Set new.
	projectMap keysAndValuesDo: [:prjClass :coll |
		coll size <= 1
			ifTrue: [ coll do: [:assoc | loadedProjectNames add: assoc key name ]]
			ifFalse: [ 
				"multiple project references against the same configuration ... only count project as loaded
				 if there is an exact match for loaded projects...
				 See http://code.google.com/p/metacello/issues/detail?id=86"
				coll do: [:assoc | | loaded packageList |
					loaded := assoc value key. "loaded packages from project"
					packageList := assoc value value. "loadlist for project"
					(packageList difference: loaded) isEmpty
						ifTrue: [ loadedProjectNames add: assoc key name ]]]].
	list := loadedPackageNames, loadedProjectNames.
	list isEmpty ifTrue: [ ^self spec defaultPackageNames].
	^list