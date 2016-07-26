system announcements
classRemoved: anAnnouncement
	| class oldPackage |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	class := anAnnouncement classRemoved.
	(self classWidget getClassesList includes: class)
		ifTrue:
			[ 
			self selectedClass
				ifNotNil:
					[ :selectedClass | 
					selectedClass theNonMetaClass = class
						ifTrue: [ self selectedClass: nil ] ].
			window isDisplayed
				ifFalse: [ ^ self ].
			oldPackage := RPackageOrganizer default
				packageMatchingExtensionName: anAnnouncement categoryName.
			self removeFromPackagesIconsCache: oldPackage.
			self updatePackageGroupAndClassList ]