image reading/writing
allTypicalFileExtensions
	"Answer a collection of file extensions (lowercase) which files that my subclasses can read might commonly have"
	"ImageReadWriter allTypicalFileExtensions"
	| extensions |
	extensions := Set new.
	self allSubclassesDo: [ :cls | extensions addAll: cls typicalFileExtensions ].
	^ extensions