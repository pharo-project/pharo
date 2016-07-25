system announcements
classDefinitionModified: anAnnouncement
	| class |
	window ifNil: [ ^ self ].
	window isDisplayed
		ifFalse: [ ^ self ].
	class := anAnnouncement oldClassDefinition.
	self removeClassFromMethodsIconsCache: class.
	(self classWidget getClassesList includes: class)
		ifTrue:
			[ 
			model selectedCategory
				ifNil:
					[ 
					model selectedMethod
						ifNil:
							[ 
							model selectedClass = anAnnouncement oldClassDefinition
								ifTrue:
									[ model selectedClass: anAnnouncement newClassDefinition ].	"Minimal change to fix Case13006. Further investigation required in Pharo 4 on Case13020"
							self updateClassView.
							self removeFromPackagesIconsCache: class package ] ] ]