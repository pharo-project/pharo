*Iceberg-Core
icePackage
	"TODO: Refactor this, see https://github.com/npasserini/iceberg/issues/42"
	self categories 
		ifNotEmpty: [ ^ Iceberg packageForCategoryNamed: (self categories anyOne) ]
	 	ifEmpty: nil
