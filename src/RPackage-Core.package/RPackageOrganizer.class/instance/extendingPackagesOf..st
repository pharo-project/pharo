package access from class
extendingPackagesOf: aClass
	"Returns the packages extending the class aClass"
	^ classExtendingPackagesMapping at: aClass theNonMetaClass name ifAbsent: [#()]