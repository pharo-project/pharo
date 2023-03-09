Extension { #name : #MCMethodDefinition }

{ #category : #'*Ring-Monticello' }
MCMethodDefinition >> asRGDefinition [

"	method2 := (Object>>#isRingObject) asRingDefinition.
	self assert: method2 class equals: RGMethod.
	self assert: method2 selector equals: #isRingObject.
	self assert: method2 parent name equals: #Object.

	self deny: (method2 environment == env1)."

	^ self ensureRingDefinitionIn: RGEnvironment new
]

{ #category : #'*Ring-Monticello' }
MCMethodDefinition >> ensureRingDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		| def classDef metaclassDef parent aProtocol |
		classDef := anRGEnvironment ensureClassNamed: self className asSymbol.

		classDef isTrait
			ifFalse: [
				metaclassDef := anRGEnvironment ensureClassNamed: (self className, ' class') asSymbol.
				classDef metaclass: metaclassDef.
				metaclassDef baseClass: classDef.]
			ifTrue: [
				metaclassDef := anRGEnvironment ensureMetaclassTraitNamed: (self className, ' classTrait') asSymbol.
				classDef classTrait: metaclassDef.
				metaclassDef baseTrait. classDef.].

		parent := self classIsMeta
			ifFalse: [ classDef ]
			ifTrue: [ metaclassDef ].

		def := RGMethod named: self selector parent: parent.
		parent addLocalMethod: def.

		def sourceCode: self source.
		def author: (self authorForStamp: self timeStamp).
		def time: (self timeForStamp: self timeStamp).

		aProtocol := parent ensureProtocolNamed: self category asSymbol.
		parent addProtocol: aProtocol.
		def protocol: aProtocol.
		def
	]
]

{ #category : #'*Ring-Monticello' }
MCMethodDefinition >> ensureRingDefinitionIn: anRGEnvironment package: anRGPackage [

	| def  |

	def := self ensureRingDefinitionIn: anRGEnvironment.
	def package: anRGPackage.

	^ def
]
