widget API
columns

	^ self model columns collect: [ :each | each buildWithSpec ]