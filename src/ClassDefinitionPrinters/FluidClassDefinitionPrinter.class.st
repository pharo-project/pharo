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

You can also configure it to display empty slots.

``` 
ClassDefinitionPrinter displayEmptySlots: true
```


"
Class {
	#name : 'FluidClassDefinitionPrinter',
	#superclass : 'ClassDefinitionPrinter',
	#category : 'ClassDefinitionPrinters',
	#package : 'ClassDefinitionPrinters'
}

{ #category : 'definition double dispatch API' }
FluidClassDefinitionPrinter >> classDefinitionString [

	"Next step
		- some of the methods defined on classe will have to be moved in this class.
		- refactor to remove duplication with metaclass,...."

	^ String streamContents: [ :s |
		  forClass superclass
			  ifNotNil: [ s nextPutAll: forClass superclass name ]
			  ifNil: [ s nextPutAll: 'nil' ].

		  self msgAndClassNameOn: s.
		  forClass classLayout isFixedLayout
				ifFalse: [ self layoutOn: s ].

			self traitsOn: s.
			self class displayEmptySlots
				ifTrue: [ self slotsOn: s ]
		  		ifFalse: [forClass slots ifNotEmpty: [ self slotsOn: s ]].
		  forClass classVariables ifNotEmpty: [ self sharedVariablesOn: s ].
		  forClass sharedPools ifNotEmpty: [ self sharedPoolsOn: s ].

		  self tagOn: s.
		  self packageOn: s ]
]

{ #category : 'template' }
FluidClassDefinitionPrinter >> classDefinitionTemplateInPackage: aPackageName tag: aTagName named: aClassName [

	^ String streamContents: [ :s |
		  s
			  nextPutAll: 'Object << ';
			  print: aClassName asSymbol;
			  crtab;
			  nextPutAll: 'layout: FixedLayout;';
			  crtab;
			  nextPutAll: 'traits: {};';
			  crtab;
			  nextPutAll: 'slots: {};';
			  crtab;
			  nextPutAll: 'sharedVariables: {};';
			  crtab;
			  nextPutAll: 'sharedPools: {};';
			  crtab;
			  nextPutAll: 'tag: ''';
			  nextPutAll: (aTagName ifNil: [ '' ]);
			  nextPutAll: ''' ;';
			  crtab;
			  nextPutAll: 'package: ''';
			  nextPutAll: aPackageName;
			  nextPutAll: '''' ]
]

{ #category : 'elementary operations' }
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

{ #category : 'template' }
FluidClassDefinitionPrinter >> compactClassDefinitionTemplateInPackage: aPackageName tag: aTagName [

	^ String streamContents: [ :s |
		  s
			  nextPutAll: 'Object << #MyClass';
			  crtab;
			  nextPutAll: 'slots: {};';
			  crtab.
		  aTagName ifNotNil: [
			  s
				  nextPutAll: 'tag: ';
				  print: aTagName;
				  nextPutAll: ';';
				  crtab ].
		  s
			  nextPutAll: 'package: ''';
			  nextPutAll: aPackageName;
			  nextPutAll: '''' ]
]

{ #category : 'template' }
FluidClassDefinitionPrinter >> compactTraitDefinitionTemplateInPackage: aPackageName tag: aTagName [

	^ String streamContents: [ :s |
		  s
			  nextPutAll: 'Trait << #TMyTrait';
			  crtab;
			  nextPutAll: 'traits: {};';
			  crtab;
			  nextPutAll: 'slots: {};';
			  crtab.
		  aTagName ifNotNil: [
			  s
				  nextPutAll: 'tag: ';
				  print: aTagName;
				  nextPutAll: ';';
				  crtab ].
		  s
			  nextPutAll: 'package: ''';
			  nextPutAll: aPackageName;
			  nextPutAll: '''' ]
]

{ #category : 'expanded double dispatch API' }
FluidClassDefinitionPrinter >> expandedDefinitionString [

	^ String streamContents: [ :s | "in case of ProtoObject"
		  forClass superclass
			  ifNil: [ s nextPutAll: 'nil' ]
			  ifNotNil: [ s nextPutAll: forClass superclass name ].
		  self msgAndClassNameOn: s.
		  self layoutOn: s.
		  s crtab.
		  s nextPutAll: 'traits: '.
		  forClass hasTraitComposition
			  ifTrue: [
				  s
					  nextPutAll: '{';
					  nextPutAll: forClass traitCompositionString;
					  nextPutAll: '};' ]
			  ifFalse: [ s nextPutAll: '{};' ].
		  self slotsOn: s.
		  self sharedVariablesOn: s.
		  self sharedPoolsOn: s.
		  forClass packageTag isRoot
			  ifTrue: [
				  s
					  crtab;
					  nextPutAll: 'tag: '''';' ]
			  ifFalse: [ self tagOn: s ].
		  self packageOn: s ]
]

{ #category : 'expanded double dispatch API' }
FluidClassDefinitionPrinter >> expandedMetaclassDefinitionString [

	^ String streamContents: [ :s |
		  forClass soleInstance = ProtoObject
			  ifTrue: [ "we are on the class of ProtoObject class.
				Yes this is strange but the fluid printer is printing
				this."
				  s nextPutAll: 'Class class << ProtoObject class' ]
			  ifFalse: [
				  s nextPutAll: forClass superclass name.
				  s nextPutAll: ' << '.
				  s nextPutAll: forClass name ].
		  s crtab.
		  forClass hasTraitComposition
			  ifTrue: [
				  s
					  nextPutAll: 'traits: ';
					  nextPutAll: forClass traitCompositionString;
					  nextPutAll: ';' ]
			  ifFalse: [ s nextPutAll: 'traits: {};' ].
		  s crtab.
		  s nextPutAll: 'slots: '.
		  self slotDefinitionsOn: s ]
]

{ #category : 'expanded double dispatch API' }
FluidClassDefinitionPrinter >> expandedTraitClassDefinitionString [

	^ String streamContents: [ :s |
		  s nextPutAll: 'Trait << '.
		  s nextPutAll: forClass name.

		  s crtab.
		  s nextPutAll: 'traits: '.
		  forClass hasTraitComposition
			  ifTrue: [
				  s
					  nextPutAll: '{';
					  nextPutAll: forClass traitCompositionString;
					  nextPutAll: '};' ]
			  ifFalse: [ s nextPutAll: '{};' ].
		  s crtab.
		  s nextPutAll: 'slots: '.
		  self slotDefinitionsOn: s ]
]

{ #category : 'expanded double dispatch API' }
FluidClassDefinitionPrinter >> expandedTraitDefinitionString [

	^ String streamContents: [ :s |
		  | tag |
		  s nextPutAll: 'Trait'.
		  self msgAndClassNameOn: s.

		  s crtab.
		  s nextPutAll: 'traits: '.
		  forClass hasTraitComposition
			  ifTrue: [
				  s
					  nextPutAll: '{';
					  nextPutAll: forClass traitCompositionString;
					  nextPutAll: '};' ]
			  ifFalse: [ s nextPutAll: '{};' ].
		  self slotsOn: s.
		  s crtab.

		  forClass packageTag ifNotNil: [ :t |
			  tag := t name.
			  tag = forClass package name ifFalse: [
				  s
					  nextPutAll: 'tag: ';
					  nextPut: $';
					  nextPutAll: tag asString;
					  nextPutAll: ''';'.
				  s crtab ] ].

		  s
			  nextPutAll: 'package: ''';
			  nextPutAll: forClass package name;
			  nextPutAll: '''' ]
]

{ #category : 'definition double dispatch API' }
FluidClassDefinitionPrinter >> expandedTraitedMetaclassDefinitionString [

	^ String streamContents:
		[:strm |
		strm
			nextPutAll: forClass superclass name;
			nextPutAll: ' << ';
			nextPutAll: forClass name.
		self traitsOn: strm.
		self lastSlotsOn: strm]
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> lastSlotsOn: s [

	s crtab.
	s nextPutAll: 'slots: '.
	self slotDefinitionsOn: s
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> lastTraitsOn: strm [
	"uses: can the last part of a definition so watch out for terminating ;"

	forClass hasTraitComposition ifTrue: [
		strm
			crtab;
			nextPutAll: 'traits: {';
			nextPutAll: forClass traitCompositionString;
			nextPutAll: '}'.
	forClass slots	ifNotEmpty: [ strm nextPutAll: ';' ] ]
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> layoutOn: s [
	"Layout is always followed by other message ultimately package: so it terminates with ;"
	s
		crtab;
		nextPutAll: 'layout: ';
		nextPutAll: forClass classLayout class name;
		nextPutAll: ';'
]

{ #category : 'definition double dispatch API' }
FluidClassDefinitionPrinter >> metaclassDefinitionString [

	^ String streamContents: [ :strm |
		forClass superclass
			ifNotNil: [ forClass = ProtoObject class
								ifFalse: [
								strm
									nextPutAll: forClass superclass name;
									nextPutAll: ' << ';
									nextPutAll: forClass name ]
								ifTrue: [ strm
									nextPutAll: 'Class class << ';
									nextPutAll: forClass name ] ]
			ifNil: [ strm nextPutAll: 'ProtoObject ' ].
		self lastTraitsOn: strm.
		self class displayEmptySlots
				ifTrue: [ self lastSlotsOn: strm ]
		  		ifFalse: [forClass slots ifNotEmpty: [ self lastSlotsOn: strm ]]]
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> msgAndClassNameOn: s [

	s
		nextPutAll: ' << #';
		nextPutAll: forClass name
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> packageOn: s [
	"The code to get the package name is not nice but it is possible we are asking the definition to a class that was uninstalled from the system and in that case it does not know its package anymore. And some code in the image expect to have the package of the class with the same name instead."

	s
		crtab;
		nextPutAll: 'package: ';
		nextPut: $';
		nextPutAll: forClass package name;
		nextPut: $'
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> sharedPoolsOn: s [
	"shared pool message is always followed by other message ultimately package: so it terminates with ;"
	s
		crtab;
		nextPutAll: 'sharedPools: {'.
	forClass sharedPools
		do: [ :p |
			s space.
			s nextPutAll: p name.
			s space ]
		separatedBy: [ s nextPutAll: '.' ].
	s
		nextPutAll: '};'
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> sharedVariablesOn: s [
	"shared variable is always followed by other message ultimately package: so it terminates with ;"
	s
		crtab;
		nextPutAll: 'sharedVariables: '.
	self classVariableDefinitionsOn: s.
	s
		nextPutAll: ';'
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> slotDefinitionString [
	"Answer a string that represents an executable description of my Slots"

	^String streamContents: [ :str | self slotDefinitionsOn: str]
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> slotDefinitionsOn: aStream [
	"If the class has many instance variable print one slot per line, do the same for complex slots."

	| oneSlotPerLine localSlots |
	"We could have the heuristics based on the sum of the slot name character length."
	oneSlotPerLine := false.
	localSlots := forClass localSlots.
	( localSlots size > 8 or: [ localSlots anySatisfy: [ :s | s needsFullDefinition ]])
		ifTrue: [ localSlots size = 1 ifFalse: [ oneSlotPerLine := true ] ].
	aStream nextPutAll: '{'.
	"To start aligned really nice when we have two complex slots"
	oneSlotPerLine ifTrue: [
		self slotShiftOn: aStream ].
	localSlots
		do: [ :slot |
			aStream space.
			aStream nextPutAll: slot definitionString.
			oneSlotPerLine := slot needsFullDefinition | oneSlotPerLine.
			aStream space ]
		separatedBy: [
			aStream nextPutAll: '.'.
			oneSlotPerLine ifTrue: [ self slotShiftOn: aStream ] ].
	aStream nextPutAll: '}'
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> slotShiftOn: aStream [

	aStream
		cr;
		tab;
		tab;
		tab
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> slotsOn: s [
	"uses: can the last part of a definition so watch out for terminating ;"

	self lastSlotsOn: s.
	s nextPutAll: ';'
]

{ #category : 'expanded double dispatch API' }
FluidClassDefinitionPrinter >> tagOn: aStream [
	"We don't print if the tag is the root tag."
	forClass packageTag isRoot ifTrue: [ ^ self ].

	aStream
		crtab;
		nextPutAll: 'tag: ''';
		nextPutAll: forClass packageTag name asString;
		nextPutAll: ''';'
]

{ #category : 'printing' }
FluidClassDefinitionPrinter >> testClassDefinitionTemplateInPackage: aPackageName tag: aTagName [

	^ String streamContents: [ :s |
		  s
			  nextPutAll: 'TestCase << #MyTest';
			  crtab;
			  nextPutAll: 'slots: {};';
			  crtab;
			  nextPutAll: 'tag: ''';
			  nextPutAll: (aTagName ifNil: [ '' ]);
			  nextPutAll: ''' ;';
			  crtab;
			  nextPutAll: 'package: ''' , aPackageName , '''' ]
]

{ #category : 'definition double dispatch API' }
FluidClassDefinitionPrinter >> traitDefinitionString [

	^ String streamContents: [ :strm |
			strm nextPutAll: 'Trait'.
			self msgAndClassNameOn: strm.
			self traitsOn: strm.

		   forClass slots ifNotEmpty: [ self slotsOn: strm ].
			self tagOn: strm.
			self packageOn: strm ]
]

{ #category : 'template' }
FluidClassDefinitionPrinter >> traitDefinitionTemplateInPackage: aPackageName tag: aTagName named: aTraitName [

	^ String streamContents: [ :s |
		  s
			  nextPutAll: 'Trait << ';
			  print: aTraitName asSymbol;
			  crtab;
			  nextPutAll: 'traits: {};';
			  crtab;
			  nextPutAll: 'slots: {};';
			  crtab;
			  nextPutAll: 'tag: ''';
			  nextPutAll: (aTagName ifNil: [ '' ]);
			  nextPutAll: ''' ;';
			  crtab;
			  nextPutAll: 'package: ''';
			  nextPutAll: aPackageName;
			  nextPutAll: '''' ]
]

{ #category : 'definition double dispatch API' }
FluidClassDefinitionPrinter >> traitedMetaclassDefinitionString [

	^ String streamContents:
		[:strm |
		strm
			nextPutAll: forClass superclass name;
			nextPutAll: ' << ';
			nextPutAll: forClass name.
		self lastTraitsOn: strm.
		forClass slots ifNotEmpty: [ self lastSlotsOn: strm ]]
]

{ #category : 'elementary operations' }
FluidClassDefinitionPrinter >> traitsOn: strm [
	"uses: can the last part of a definition so watch out for terminating ;"

	forClass hasTraitComposition ifTrue: [
		"forClass traitComposition innerClass = TEmpty ifTrue: [ ^ self ]."
		"isEmpty traitComposition should probably moved to Trait."
		strm
			crtab;
			nextPutAll: 'traits: {';
			nextPutAll: forClass traitCompositionString;
			nextPutAll: '};' ]
]
