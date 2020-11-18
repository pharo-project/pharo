Class {
	#name : #OldPharoClassDefinitionPrinter,
	#superclass : #ClassDefinitionPrinter,
	#category : #'Kernel-ClassDefinitionPrinter'
}

{ #category : #internal }
OldPharoClassDefinitionPrinter >> basicClassDefinitionString [

	"Pay attention this definition is fragile in presence of complex slots. Use the public API i.e., classDefinitionString"

	| poolString stream |
	poolString := forClass sharedPoolsString.
	stream := (String new: 800) writeStream.
	forClass superclass
		ifNotNil: [ stream nextPutAll: forClass superclass name ]
		ifNil: [ stream nextPutAll: 'ProtoObject' ].
	stream
		nextPutAll: forClass kindOfSubclass;
		store: forClass name.
	self traitCompositionOn: stream.
	self instanceVariablesOn: stream.
	self classVariablesOn: stream.
	poolString = '' ifFalse: [ 
		stream
			crtab;
			nextPutAll: 'poolDictionaries: ';
			store: poolString ].
	self packageOn: stream.
	forClass superclass ifNil: [ 
		stream
			nextPutAll: '.';
			cr.
		stream nextPutAll: forClass name.
		stream
			space;
			nextPutAll: 'superclass: nil' ].
	^ stream contents
]

{ #category : #internal }
OldPharoClassDefinitionPrinter >> basicMetaclassDefinitionString [

	"Pay attention this definition is fragile in presence of complex slots. Use the public API i.e., classDefinitionString"

	^ String streamContents: [ :stream | 
		  stream print: forClass.
		  self instanceVariablesOn: stream ]
]

{ #category : #internal }
OldPharoClassDefinitionPrinter >> basicTraitDefinitionString [

	"Pay attention this definition is fragile in presence of complex slots. Use the public API i.e., classDefinitionString"

	forClass instanceSide name == #Trait ifTrue: [ 
		^ self classDefinitionString ].

	^ String streamContents: [ :s | 
		  s
			  nextPutAll: 'Trait named: ';
			  nextPutAll: forClass name printString.
		  self traitCompositionOn: s.
		  "Important"
		  self flag: #todo.
		  "this class definition should NOT use slots: 
			either this is a simple slot and we use instance variable names
			or this is a complex slot and we use the fluid definition.
			we could have forClass neslotsNeedFullDefinition
					ifTrue: [ (self class fluid forClass: self) traitDefinitionString ] "
		  forClass classLayout visibleSlots ifNotEmpty: [ 
			  s
				  tab;
				  nextPutAll: ' slots: ';
				  nextPutAll: forClass slotDefinitionString;
				  cr ].
		  "End of important"
		  self packageOn: s ]
]

{ #category : #internal }
OldPharoClassDefinitionPrinter >> basicTraitedMetaclassDefinitionString [ 

	^ String streamContents: [:strm |
			strm print: forClass.
			self traitCompositionOn: strm.
			self instanceVariablesOn: strm ]
]

{ #category : #'public API' }
OldPharoClassDefinitionPrinter >> classDefinitionString [

	^ forClass needsSlotClassDefinition 
		ifTrue: [ (ClassDefinitionPrinter fluid for: forClass) classDefinitionString ]
		ifFalse: [ self basicClassDefinitionString ]
]

{ #category : #template }
OldPharoClassDefinitionPrinter >> classDefinitionTemplateInPackage: aString [

	^ String streamContents: [ :s |
			s nextPutAll: 'Object subclass: #MyClass' ; crtab.
			s nextPutAll: 'instanceVariableNames: '''' '; crtab. 
			s nextPutAll: 'classVariableNames: '''''; crtab.
			s nextPutAll: 'package: ''', aString, '''' ]
]

{ #category : #internal }
OldPharoClassDefinitionPrinter >> classVariablesOn: stream [

	stream
		crtab;
		nextPutAll: 'classVariableNames: '''.
	forClass classVariablesOn: stream.
	stream nextPut: $'
]

{ #category : #template }
OldPharoClassDefinitionPrinter >> compactClassDefinitionTemplateInPackage: aString [
	"there is not compact version..."

	^ self classDefinitionTemplateInPackage: aString
]

{ #category : #template }
OldPharoClassDefinitionPrinter >> compactTraitDefinitionTemplateInPackage: aString [

	^ self traitDefinitionTemplateInPackage: aString
]

{ #category : #'public API' }
OldPharoClassDefinitionPrinter >> expandedDefinitionString [
	"We do not support expanded on old Pharo syntax so we shortcut the double dispatch call."
	^ self definitionString 
]

{ #category : #internal }
OldPharoClassDefinitionPrinter >> instanceVariablesOn: stream [

	stream
		crtab;
		nextPutAll: 'instanceVariableNames: '''.
	forClass instanceVariablesOn: stream.
	stream nextPut: $'
]

{ #category : #'public API' }
OldPharoClassDefinitionPrinter >> metaclassDefinitionString [ 

	^ forClass needsSlotClassDefinition 
		ifTrue: [ (ClassDefinitionPrinter fluid for: forClass) metaclassDefinitionString ]
		ifFalse: [ self basicMetaclassDefinitionString ]
]

{ #category : #internal }
OldPharoClassDefinitionPrinter >> packageOn: stream [

	stream
		crtab;
		nextPutAll: 'package: ';
		store: forClass category asString
]

{ #category : #internal }
OldPharoClassDefinitionPrinter >> traitCompositionOn: stream [

	forClass hasTraitComposition ifTrue: [ 
		stream
			crtab;
			nextPutAll: 'uses: ';
			nextPutAll: forClass traitCompositionString ]
]

{ #category : #'public API' }
OldPharoClassDefinitionPrinter >> traitDefinitionString [ 

	^ forClass needsSlotClassDefinition 
		ifTrue: [ (ClassDefinitionPrinter fluid for: forClass) traitDefinitionString ]
		ifFalse: [ self basicTraitDefinitionString ]
]

{ #category : #template }
OldPharoClassDefinitionPrinter >> traitDefinitionTemplateInPackage: aString [

	^ String streamContents: [ :s |
			s nextPutAll: 'Trait named: #TMyTraits
	 uses: {}
	 package: '''.
			s nextPutAll: aString; nextPut: $' ]
 
]

{ #category : #'public API' }
OldPharoClassDefinitionPrinter >> traitedMetaclassDefinitionString [ 

	^ forClass needsSlotClassDefinition 
		ifTrue: [ (ClassDefinitionPrinter fluid for: forClass) traitedMetaclassDefinitionString ]
		ifFalse: [ self basicTraitedMetaclassDefinitionString ]
]
