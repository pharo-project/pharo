Usage: save <imageBaseName> [--delete-old]
 	<imageName>     a base name for the image
	--delete-old    remove the old image and changes file
	
Documentation:
Saves the image and changes file under a new name.

Examples:
	# create a 'bar.image' and 'foo.changes'
	pharo Pharo.image save bar
	# create the same file as in the previous example but delete Foo.image and Foo.changes
	pharo Pharo.image save bar --delete-old