Extension { #name : #MCClassDefinition }

{ #category : #'*Ring-Monticello' }
MCClassDefinition >> asRGDefinition [

	^ self ensureRingDefinitionIn: RGEnvironment new
]

{ #category : #'*Ring-Monticello' }
MCClassDefinition >> ensureRingDefinitionIn: anRGEnvironment [

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		| def metaclassDef |
		def := anRGEnvironment ensureClassNamed: self className asSymbol.
		metaclassDef := anRGEnvironment ensureClassNamed: (self className, ' class') asSymbol.
		def metaclass: metaclassDef.
		def traitComposition: (self traitCompositionFrom: self traitComposition parent: def).			
		metaclassDef traitComposition: (self traitCompositionFrom: self classTraitComposition parent: metaclassDef).			
		(self superclassName = #nil)
			ifTrue: [ def superclass: def ]
			ifFalse: [ def superclass: (anRGEnvironment ensureClassNamed: superclassName ) ].
		def category: self category.
		def layout: (self ring2LayoutType parent: def).
		metaclassDef layout: (RGFixedLayout parent: metaclassDef). 
		def comment: (
			(RGComment parent: def) 
				content: self comment;
				author: (self authorForStamp: self commentStamp);
				time: (self timeForStamp: self commentStamp);
				yourself).
		self variables do: [ :each |
			(each class type = #instance) ifTrue: [
				(each name includes: $>) 
					ifFalse: [ def layout addSlot: (RGInstanceVariableSlot named: each name parent: def layout)]
					ifTrue: [ 
						| unknownSlot |
						unknownSlot := RGUnknownSlot named: ((each name copyUpTo: $=) copyWithout: $#) trimBoth parent: def layout.
						unknownSlot expression: (each name copyAfter: $>) trimBoth.
						def layout addSlot: unknownSlot]].
			(each class type = #class) ifTrue: [ 
				def addClassVariable: (RGClassVariable named: each name parent: def)].
			(each class type = #classInstance) ifTrue: [ 
				metaclassDef layout addSlot: (RGInstanceVariableSlot named: each name parent: metaclassDef layout)].
			(each class type = #pool) ifTrue: [ 
				def addSharedPool: (RGPoolVariable named: each name parent: def)]. ]. 
		def]. 
	
]

{ #category : #'*Ring-Monticello' }
MCClassDefinition >> ensureRingDefinitionIn: anRGEnvironment package: anRGPackage [
	
	| def |
	
	def := self ensureRingDefinitionIn: anRGEnvironment.
	def package: anRGPackage.
	def category: def tags first. "fix tags to do not contain package names"
	^ def
]

{ #category : #'*Ring-Monticello' }
MCClassDefinition >> ring2LayoutType [

	^ (Dictionary newFrom: {	
		#compiledMethod -> RGCompiledMethodLayout. 
		#bytes -> RGByteLayout. 
		#immediate -> RGImmediateLayout. 
		#words -> RGWordLayout. 
		#week -> RGWeakLayout.
		#variable -> RGVariableLayout.
		#ephemeron -> RGEphemeronLayout.
		#normal -> RGFixedLayout.
		#weak -> RGWeakLayout.
	}) at: type.		

]
