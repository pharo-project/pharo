private
buildWidget

	^ self menuItems collect: [ :each | each buildWithSpec ]