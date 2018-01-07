Extension { #name : #MCDefinition }

{ #category : #'*Ring2-Monticello' }
MCDefinition >> authorForStamp: stamp [
	
	^ stamp isEmptyOrNil
		ifTrue: [ '<historical>']
		ifFalse: [ stamp substrings first].
]

{ #category : #'*Ring2-Monticello' }
MCDefinition >> ensureRing2DefinitionIn: aRG2Environment [

	"do nothing by default"
]

{ #category : #'*Ring2-Monticello' }
MCDefinition >> ensureRing2DefinitionIn: aRG2Environment package: aRG2PackageDefinition [
	
	"by default do not set the package"

	^ self ensureRing2DefinitionIn: aRG2Environment.
	
]

{ #category : #'*Ring2-Monticello' }
MCDefinition >> timeForStamp: stamp [

	^ stamp
		ifNil: [ 	DateAndTime new]
		ifNotNil: [ 
			| substrings |
			substrings := stamp substrings.
			([(DateAndTime fromString: (' ' join: substrings allButFirst ))] on: Exception do: [DateAndTime epoch]) ]. 
	
]

{ #category : #'*Ring2-Monticello' }
MCDefinition >> traitCompositionFrom: aStringOrNil parent: aBehavior [
	
	| composition transformations |
	
	composition := RG2TraitCompositionDefinition parent: aBehavior.

	aStringOrNil ifNil: [ ^ composition ].

	transformations := RG2TraitCompositionVisitor new parse: aStringOrNil for: composition.
	transformations do: [ :each |
		composition addTransformation: each ].
	^ composition
]
