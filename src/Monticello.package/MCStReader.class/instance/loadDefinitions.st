evaluating
loadDefinitions
	| filePackage |
	filePackage := RingChunkImporter fromStream: self readStream.
	definitions := OrderedCollection new.
	filePackage classes
		do: [ :ringClass |
			ringClass hasDefinitionSource
				ifTrue: [definitions add:
						(self classDefinitionFrom: ringClass)].
			definitions addAll: (self methodDefinitionsFor: ringClass).
			definitions addAll: (self methodDefinitionsFor: ringClass theMetaClass) ].
	filePackage doIts 
		do: [ :ea | self addDefinitionsFromDoit: ea contents ].
	