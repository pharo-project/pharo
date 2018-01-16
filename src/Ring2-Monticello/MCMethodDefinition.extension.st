Extension { #name : #MCMethodDefinition }

{ #category : #'*Ring2-Monticello' }
MCMethodDefinition >> asRing2Definition [

"	method2 := (Object>>#isRing2Object) asRing2Definition.
	self assert: method2 class equals: RG2MethodDefinition.	
	self assert: method2 selector equals: #isRing2Object.
	self assert: method2 parent name equals: #Object.

	self deny: (method2 environment == env1)."

	^ self ensureRing2DefinitionIn: RG2Environment new
	

]

{ #category : #'*Ring2-Monticello' }
MCMethodDefinition >> ensureRing2DefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		| def classDef metaclassDef parent protocol |
		classDef := aRG2Environment ensureClassNamed: self className asSymbol.
		
		classDef isTrait 
			ifFalse: [ 
				metaclassDef := aRG2Environment ensureClassNamed: (self className, ' class') asSymbol.
				classDef metaclass: metaclassDef.
				metaclassDef baseClass: classDef.]
			ifTrue: [ 
				metaclassDef := aRG2Environment ensureMetaclassTraitNamed: (self className, ' classTrait') asSymbol.
				classDef classTrait: metaclassDef.
				metaclassDef baseTrait. classDef.].
				
		parent := self classIsMeta 
			ifFalse: [ classDef ]
			ifTrue: [ metaclassDef ].
		
		def := RG2MethodDefinition named: self selector parent: parent.
		parent addLocalMethod: def.
			
		def sourceCode: self source.
		def author: (self authorForStamp: self timeStamp).
		def time: (self timeForStamp: self timeStamp).
		
		protocol := parent ensureProtocolNamed: self category asSymbol.
		parent addProtocol: protocol.
		def protocol: protocol.
		def 
	]
]

{ #category : #'*Ring2-Monticello' }
MCMethodDefinition >> ensureRing2DefinitionIn: aRG2Environment package: aRG2PackageDefinition [
	
	| def  |
	
	def := self ensureRing2DefinitionIn: aRG2Environment.
	def package: aRG2PackageDefinition.

	^ def

]
