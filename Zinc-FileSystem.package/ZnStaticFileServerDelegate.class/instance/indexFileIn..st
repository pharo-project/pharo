private
indexFileIn: fileDirectory
	#( 'index.html' 'index.htm' ) do: [ :each | | file |
			(file := fileDirectory / each) exists
				ifTrue: [ 
					^ file fullName ] ].
	^ nil
