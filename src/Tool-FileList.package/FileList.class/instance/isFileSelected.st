private
isFileSelected
	"return if a file is currently selected"

	^ reference notNil and: [ reference isFile ].