Extension { #name : #MCClassDefinition }

{ #category : #'*Iceberg-Adapters' }
MCClassDefinition >> classCategory [
	"Avoid direct use of #category, because MCClassDefinitions and 
	MCMethodDefinition use the same message for different things. "
	^ self category
]

{ #category : #'*Iceberg' }
MCClassDefinition >> fullDefinitionString [
	^ String streamContents: [:stream | 
		self printDefinitionOn: stream.
		stream cr; cr.
		stream << self comment ]
]
