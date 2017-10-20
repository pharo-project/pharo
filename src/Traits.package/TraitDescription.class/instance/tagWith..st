accessing tags
tagWith: aSymbol
	| package packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we could only model single tag with RPackageTag"
	
	package := self package.
	packageTag := package addClassTag: aSymbol.
	packageTag addClass: self