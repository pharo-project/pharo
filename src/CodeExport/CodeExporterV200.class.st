Class {
	#name : 'CodeExporterV200',
	#superclass : 'CodeExporterV100',
	#category : 'CodeExport-Base',
	#package : 'CodeExport',
	#tag : 'Base'
}

{ #category : 'writing - low level' }
CodeExporterV200 >> writeClassDefinition: aClassDefinition [

	writeStream cr.
	writeStream bang.
	writeStream nextPut: #BehaviorDefinition.

	writeStream cr.
	writeStream nextPut: aClassDefinition
]

{ #category : 'writing - low level' }
CodeExporterV200 >> writeClassDefinitionOf: aClass [ 

	self writeClassDefinition: aClass definitionString
]

{ #category : 'writing' }
CodeExporterV200 >> writeFileoutVersion [

	writeStream nextPut: '"VERSION:2.0.0"'
]

{ #category : 'writing - low level' }
CodeExporterV200 >> writePackageDefinitionOf: aPackage [

	writeStream cr.
	writeStream bang.
	writeStream nextPut: #PackageDefinition.

	writeStream cr.
	writeStream nextPut: aPackage name
]
