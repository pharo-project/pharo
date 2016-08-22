menus behavior
restrictedBrowseGroups: aCollection
	aCollection
		ifNotEmpty: [ | environment |
			environment := aCollection allButFirst
				inject: (aCollection first restrict: self browsedEnvironment)
				into: [ :env :group | env | (group restrict: self browsedEnvironment) ].
			self model class openOnGroup: aCollection first inEnvironment: environment ]