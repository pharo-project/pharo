"
```
Object < #MyClass
	layout: FixedLayout;
	uses: {};
	slots: {};
	sharedVariables: {};
	sharedPools: {};
	tag: '' ;
	package: 'FluidClassDefinition-Printer-UnderDev'
```

is the template definition for the fluid syntax


This is the fluid definition of the current class :).

```
ClassDefinitionPrinter < #FluidClassDefinitionPrinter
	tag: 'Printer-UnderDev';
	package: 'FluidClassDefinition'
```


"
Class {
	#name : #FluidClassDefinitionPrinter,
	#superclass : #ClassDefinitionPrinter,
	#category : #Kernel
}

{ #category : #running }
FluidClassDefinitionPrinter >> classDefinitionParserClass [

	^ CDFluidClassDefinitionParser
]

{ #category : #'definition double dispatch API' }
FluidClassDefinitionPrinter >> classDefinitionString [

	"Next step
		- some of the methods defined on classe will have to be moved in this class.
		- refactor to remove duplication with metaclass,...."

	^ String streamContents: [ :s | 
		  forClass superclass
			  ifNotNil: [ s nextPutAll: forClass superclass name ]
			  ifNil: [ s nextPutAll: 'nil' ].
		  s
			  nextPutAll: ' < #';
			  nextPutAll: forClass name;
			  cr.

		  forClass classLayout isFixedLayout ifFalse: [ self layoutOn: s ].

		  forClass hasTraitComposition ifTrue: [ 
			  s tab.
			  self traitCompositionOn: s.
			  s cr ].

		  forClass slots ifNotEmpty: [ self slotsOn: s ].
		  forClass classVariables ifNotEmpty: [ self sharedVariablesOn: s ].
		  forClass sharedPools ifNotEmpty: [ self sharedPoolsOn: s ].

		  self tagOn: s.
		  self packageOn: s ]
]

{ #category : #printing }
FluidClassDefinitionPrinter >> classDefinitionTemplateInPackage: aPackageName [

		^ String streamContents: [ :s |
						s nextPutAll: 'Object < #MyClass'; crtab.
						s nextPutAll: 'layout: FixedLayout;'; crtab.
						s nextPutAll: 'uses: {};'; crtab.
						s nextPutAll: 'slots: {};'; crtab. 
						s nextPutAll: 'sharedVariables: {};'; crtab.
						s nextPutAll: 'sharedPools: {};'; crtab.
						s nextPutAll: 'tag: '''' ;';crtab.
						s nextPutAll: 'package: ''', aPackageName, '''' ] 
]

{ #category : #'elementary operations' }
FluidClassDefinitionPrinter >> classVariableDefinitionsOn: aStream [ 
	"Answer a string that evaluates to the definition of the class Variables"
	
		| fullDef |
		forClass classVariables 
			ifEmpty: [ aStream nextPutAll: '{}' ]
			ifNotEmpty:[  :clsVars|
		aStream nextPutAll: '{'.
		clsVars do: [:global |
				aStream space. 
				aStream nextPutAll: global definitionString.
				fullDef := global needsFullDefinition.
				aStream space.]				
			separatedBy: [ 
				aStream nextPutAll: '.'.  
				fullDef ifTrue: [ aStream cr;tab;tab;tab;tab ]].
		aStream nextPutAll: '}'. ]
]

{ #category : #'expanded double dispatch API' }
FluidClassDefinitionPrinter >> expandedClassDefinitionString [

	^ String streamContents: [ :s | 
		  s
			  nextPutAll: 'Object < #' , forClass name asSymbol;
			  cr.
		  self layoutOn: s.
		  s tab.
		  forClass hasTraitComposition
			  ifTrue: [ self traitCompositionOn: s ]
			  ifFalse: [ s nextPutAll: 'uses: {};' ].
		  s cr.
		  self slotsOn: s.
		  self sharedVariablesOn: s.
		  forClass hasTraitComposition.
		  self sharedPoolsOn: s.
		
		(forClass package classTagForClass: forClass) ifNotNil: [ :t |
			| tag | 
			tag := t name.
			tag = forClass package name 
				ifTrue: [ s tab; nextPutAll: 'tag: '''';'; cr ]
				ifFalse: [ self tagOn: s]].
		  self packageOn: s ]
]

{ #category : #'expanded double dispatch API' }
FluidClassDefinitionPrinter >> expandedMetaclassDefinitionString [
	
	^ String streamContents: [ :s | 
						forClass  
							ifNotNil: [ s nextPutAll: forClass name ]
							ifNil: [ s nextPutAll: 'ProtoObject ' ].
						
						s crtab. 
						
						forClass hasTraitComposition 
							ifTrue: [
								s 
									nextPutAll: 'uses: ';
									nextPutAll: forClass traitCompositionString;
									nextPutAll: ';']
							ifFalse: [ s nextPutAll: 'uses: {};'].
						s crtab.

						s nextPutAll: 'slots: '.
						self slotDefinitionsOn: s. 
						]
]

{ #category : #'expanded double dispatch API' }
FluidClassDefinitionPrinter >> expandedTraitDefinitionString [
	
	^ String streamContents: [ :s |		
		| tag |
		s 
			nextPutAll: 'Trait  << #';
			nextPutAll: forClass name;
			cr.

			forClass hasTraitComposition 
							ifTrue: [
								s 
									nextPutAll: 'uses: ';
									nextPutAll: forClass traitCompositionString ]
							ifFalse: [ s nextPutAll: 'uses: {};'].
						s crtab.

				s nextPutAll: 'slots: ', forClass slotDefinitionString, ';'; crtab. 
						s 	
							nextPutAll: 'sharedVariables: ';
							nextPutAll: forClass classVariableDefinitionString;
							nextPutAll: ';'; crtab.
							
						s nextPutAll: 'sharedPools: {'.
						self sharedPoolsOn: s.
						s nextPutAll: '};' ; crtab.
						
						(forClass package classTagForClass: forClass) ifNotNil: [:t | 
							tag := t name.
							tag = forClass package name
								ifFalse:  [
									s nextPutAll: 'tag: '; 
					nextPut: $'; 
					nextPutAll: tag asString ; 
					nextPutAll: ''';'.
					s crtab
				]].
				
				s 
					nextPutAll: 'package: ''';
					nextPutAll: forClass package packageName;
					nextPutAll: '''' ]
]

{ #category : #'elementary operations' }
FluidClassDefinitionPrinter >> layoutOn: s [

	s
		tab;
		nextPutAll: 'layout: ';
		nextPutAll: forClass classLayout class name;
		nextPutAll: ';';
		cr
]

{ #category : #'definition double dispatch API' }
FluidClassDefinitionPrinter >> metaclassDefinitionString [ 

	^ String streamContents: [ :s |
		
		forClass superclass 
			ifNotNil: [ s nextPutAll: forClass name ]
			ifNil: [ s nextPutAll: 'ProtoObject ' ].
		
		forClass hasTraitComposition ifTrue: [
			s 
				crtab; 
				nextPutAll: 'uses: ';
				nextPutAll: forClass traitCompositionString ;
				nextPutAll: ';';
				cr ].
		
		forClass slots ifNotEmpty: 
			[ 
				s crtab.
				s nextPutAll: 'slots: '.
				forClass slotDefinitionsOn: s ].
		]
]

{ #category : #'definition double dispatch API' }
FluidClassDefinitionPrinter >> packageOn: s [

	s
		tab;
	 	nextPutAll: 'package: ';
		nextPut: $';
		nextPutAll: forClass package packageName;
		nextPut: $'
]

{ #category : #'elementary operations' }
FluidClassDefinitionPrinter >> sharedPoolsOn: s [

	s
		tab;
		nextPutAll: 'sharedPools: {'.
	forClass sharedPools
		do: [ :p | 
			s space.
			s nextPutAll: p name.
			s space ]
		separatedBy: [ s nextPutAll: '.' ].
	s
		nextPutAll: '};';
		cr
]

{ #category : #'elementary operations' }
FluidClassDefinitionPrinter >> sharedVariablesOn: s [

	s
		tab;
		nextPutAll: 'sharedVariables: '.
	self classVariableDefinitionsOn: s.
	s  
		nextPutAll: ';';
		cr
]

{ #category : #'elementary operations' }
FluidClassDefinitionPrinter >> slotDefinitionsOn: aStream [
	"Write on the arg aStream an executable description of my Slots"

	| useFull |
	aStream nextPutAll: '{'.
	forClass localSlots
		do: [ :slot | 
			aStream space.
			aStream nextPutAll: slot definitionString.
			useFull := slot needsFullDefinition.
			aStream space ]
		separatedBy: [ 
			aStream nextPutAll: '.'.
			useFull ifTrue: [ 
				aStream
					cr;
					tab;
					tab;
					tab;
					tab ] ].
	aStream nextPutAll: '}'
]

{ #category : #'elementary operations' }
FluidClassDefinitionPrinter >> slotsOn: s [

	s tab.
	s nextPutAll: 'slots: '.
	self slotDefinitionsOn: s.
	s nextPutAll: ';'.
	s cr
]

{ #category : #'expanded double dispatch API' }
FluidClassDefinitionPrinter >> tagOn: s [

	| tag |
	(forClass package classTagForClass: forClass) ifNotNil: [ :t | 
		tag := t name.
		tag = forClass package name ifFalse: [ 
			s tab.
			s
				nextPutAll: 'tag: ';
				nextPut: $';
				nextPutAll: tag asString;
				nextPutAll: ''';'.
			s cr ] ]
]

{ #category : #'expanded double dispatch API' }
FluidClassDefinitionPrinter >> traitCompositionOn: s [

	s
		nextPutAll: 'uses: ';
		nextPutAll: forClass traitCompositionString;
		nextPutAll: ';'
]

{ #category : #'definition double dispatch API' }
FluidClassDefinitionPrinter >> traitDefinitionString [ 

	^ String streamContents: [ :s |		
		s 
			nextPutAll: 'Trait  << #';
			nextPutAll: forClass name;
			cr.
		
		forClass hasTraitComposition ifTrue: [
			s 
				tab; 
				nextPutAll: 'uses: ';
				nextPutAll: forClass traitCompositionString ;
				nextPutAll: ';';
				cr ].
		
		forClass slots ifNotEmpty: [ 
					s tab.
					s nextPutAll: 'slots: '.
					forClass slotDefinitionsOn: s.
					s nextPutAll: ';'.
					s cr ].
				
		self tagOn: s.
		
		s 	
			tab;
			nextPutAll: 'package: '; 
			nextPut: $';
			nextPutAll: forClass package packageName;
			nextPut: $'
		]
]

{ #category : #printing }
FluidClassDefinitionPrinter >> traitDefinitionTemplateInPackage: aPackageName [
	
	^ String streamContents: [ :s | 
		  s
			  nextPutAll: 'Trait << #TMyTrait';
			  crtab.
		  s
			  nextPutAll: 'uses: {};';
			  crtab.
		  s
			  nextPutAll: 'slots: {};';
			  crtab.
		  s
			  nextPutAll: 'tag: '''' ;';
			  crtab.
		  s
			  nextPutAll: 'package: ''';
			  nextPutAll: aPackageName;
			  nextPutAll: '''' ]
]

{ #category : #'definition double dispatch API' }
FluidClassDefinitionPrinter >> traitedMetaclassDefinitionString [

	^ String streamContents: 
		[:strm |
		strm nextPutAll: forClass name.
		
		forClass hasTraitComposition ifTrue: [
			strm
				crtab;
				nextPutAll: 'uses: ';
				print: forClass traitComposition ].
		forClass slots ifNotEmpty: [  
			strm
				crtab;
				nextPutAll: 'slots: '.
			forClass slotDefinitionsOn: strm	]]
]
