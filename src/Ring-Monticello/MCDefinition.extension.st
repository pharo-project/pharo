Extension { #name : #MCDefinition }

{ #category : #'*Ring-Monticello' }
MCDefinition >> ensureRingDefinitionIn: anRGEnvironment [

	"do nothing by default"
]

{ #category : #'*Ring-Monticello' }
MCDefinition >> ensureRingDefinitionIn: anRGEnvironment package: anRGPackage [

	"by default do not set the package"

	^ self ensureRingDefinitionIn: anRGEnvironment
]
