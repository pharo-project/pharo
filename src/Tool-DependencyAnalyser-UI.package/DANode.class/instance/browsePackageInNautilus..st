browse-nautilus
browsePackageInNautilus: aPackage
	| classToBrowse classFromPackage classesToBrowse |
	classFromPackage := aPackage classes.
	classesToBrowse := classFromPackage reject: [ :class | classFromPackage includes: class superclass ].
	classToBrowse := classesToBrowse isEmpty
		ifFalse: [ (classesToBrowse sorted: [ :a :b | a asString < b asString ]) first ].
	classToBrowse ifNotNil: [ self browseClassInNautilus: classToBrowse ]