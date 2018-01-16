Extension { #name : #Trait }

{ #category : #'*Ring2-Compatibility' }
Trait >> asRing2Definition [

	| model env |
	
	model := (RG2TraitDefinition named: self name).
	env := model environment.
	model 	superclass: (env ensureClassNamed: #Trait).
		
	self tags do: [ :aTag | model tagWith: aTag ].
	
	"TODO"

	^ model
]

{ #category : #'*Ring2-Compatibility' }
Trait >> asRing2DefinitionIn: aRG2Environment [

	^ RG2TraitDefinition named: self name parent: aRG2Environment.

]
