private
importClass: aClass inTag: aTag
	"import a class already created but not attached to a package to the receiver. 
	Handle also *- convention. Methods defined in *category are not added to the package.
	Pay attention that it will not import anything from the metaClass side"

	self basicImportClass: aClass.
	self basicImportClass: aClass theMetaClass.	
	self 
		addClassDefinition:  aClass
		toClassTag: aTag name