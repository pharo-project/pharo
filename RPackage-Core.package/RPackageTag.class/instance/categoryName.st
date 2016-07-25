accessing
categoryName 
	"Answer this tag as a SystemCategory. 
    That would be RPackage, '-', RPackageTag, unless tag is root (same name as RPackage)"
	^ self toCategoryName: self name
