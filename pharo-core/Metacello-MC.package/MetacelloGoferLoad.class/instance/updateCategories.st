private
updateCategories
	MetacelloPlatform current bypassGoferLoadUpdateCategories
		ifFalse: [ super updateCategories ]