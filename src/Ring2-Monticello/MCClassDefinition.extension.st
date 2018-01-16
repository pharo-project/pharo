Extension { #name : #MCClassDefinition }

{ #category : #'*Ring2-Monticello' }
MCClassDefinition >> asRing2Definition [

	^ self ensureRing2DefinitionIn: RG2Environment new
]

{ #category : #'*Ring2-Monticello' }
MCClassDefinition >> ensureRing2DefinitionIn: aRG2Environment [

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		| def metaclassDef |
		def := aRG2Environment ensureClassNamed: self className asSymbol.
		metaclassDef := aRG2Environment ensureClassNamed: (self className, ' class') asSymbol.
		def metaclass: metaclassDef.
		def traitComposition: (self traitCompositionFrom: self traitComposition parent: def).			
		metaclassDef traitComposition: (self traitCompositionFrom: self classTraitComposition parent: metaclassDef).			
		(self superclassName = #nil)
			ifTrue: [ def superclass: def ]
			ifFalse: [ def superclass: (aRG2Environment ensureClassNamed: superclassName ) ].
		def category: self category.
		def layout: (self ring2LayoutType parent: def).
		metaclassDef layout: (RG2FixedLayoutDefinition parent: metaclassDef). 
		def comment: (
			(RG2CommentDefinition parent: def) 
				content: self comment;
				author: (self authorForStamp: self commentStamp);
				time: (self timeForStamp: self commentStamp);
				yourself).
		self variables do: [ :each |
			(each class type = #instance) ifTrue: [
				(each name includes: $>) 
					ifFalse: [ def layout addSlot: (RG2InstanceVariableSlotDefinition named: each name parent: def layout)]
					ifTrue: [ 
						| unknownSlot |
						unknownSlot := RG2UnknownSlotDefinition named: ((each name copyUpTo: $=) copyWithout: $#) trimBoth parent: def layout.
						unknownSlot expression: (each name copyAfter: $>) trimBoth.
						def layout addSlot: unknownSlot]].
			(each class type = #class) ifTrue: [ 
				def addClassVariable: (RG2ClassVariableDefinition named: each name parent: def)].
			(each class type = #classInstance) ifTrue: [ 
				metaclassDef layout addSlot: (RG2InstanceVariableSlotDefinition named: each name parent: metaclassDef layout)].
			(each class type = #pool) ifTrue: [ 
				def addSharedPool: (RG2PoolVariableDefinition named: each name parent: def)]. ]. 
		def]. 
	
]

{ #category : #'*Ring2-Monticello' }
MCClassDefinition >> ensureRing2DefinitionIn: aRG2Environment package: aRG2PackageDefinition [
	
	| def |
	
	def := self ensureRing2DefinitionIn: aRG2Environment.
	def package: aRG2PackageDefinition.
	def category: def tags first. "fix tags to do not contain package names"
	^ def
]

{ #category : #'*Ring2-Monticello' }
MCClassDefinition >> ring2LayoutType [

	^ (Dictionary newFrom: {	
		#compiledMethod -> RG2CompiledMethodLayoutDefinition. 
		#bytes -> RG2ByteLayoutDefinition. 
		#immediate -> RG2ImmediateLayoutDefinition. 
		#words -> RG2WordLayoutDefinition. 
		#week -> RG2WeakLayoutDefinition.
		#variable -> RG2VariableLayoutDefinition.
		#ephemeron -> RG2EphemeronLayoutDefinition.
		#normal -> RG2FixedLayoutDefinition.
		#weak -> RG2WeakLayoutDefinition.
	}) at: type.		

]
