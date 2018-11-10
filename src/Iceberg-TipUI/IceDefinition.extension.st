Extension { #name : #IceDefinition }

{ #category : #'*Iceberg-TipUI' }
IceDefinition >> icon [
	
	self subclassResponsibility
]

{ #category : #'*Iceberg-TipUI' }
IceDefinition >> iconNamed: aSymbol [
	^ Smalltalk ui icons iconNamed: aSymbol
]
