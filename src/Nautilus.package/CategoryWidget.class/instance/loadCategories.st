private
loadCategories
	| class |
	class := self model selectedClass.
	class ifNil: [ ^ #() ].
	^ self loadPackagesCategoriesFor: class