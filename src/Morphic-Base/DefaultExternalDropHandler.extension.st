Extension { #name : #DefaultExternalDropHandler }

{ #category : #'*Morphic-Base' }
DefaultExternalDropHandler >> handle: aFileReference in: pasteUp dropEvent: anEvent [
	"the file was just droped, let's do our job"

	| fileName services theOne |
	fileName := aFileReference fullName.
	services := self servicesForFileNamed: fileName.

	"no service, default behavior"
	services isEmpty
		ifTrue: [ aFileReference inspect.
			^ self ].
	theOne := self chooseServiceFrom: services.
	theOne ifNotNil: [ theOne performServiceFor: aFileReference ]
]
