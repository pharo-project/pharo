"
This prints the class definition as it was till Pharo9: ST80 style, but with extensions/changes:
- package: instead of category:
- if no pool is imported, the pool import is not printed
- support for traits
"
Class {
	#name : 'OldPharoClassDefinitionPrinter',
	#superclass : 'ClassDefinitionPrinter',
	#category : 'ClassDefinitionPrinters',
	#package : 'ClassDefinitionPrinters'
}

{ #category : 'internal' }
OldPharoClassDefinitionPrinter >> applyCustomizersOn: stream [

	self class customizers do: [ :customizer | customizer oldClassDefinitionFor: forClass on: stream ]
]

{ #category : 'internal' }
OldPharoClassDefinitionPrinter >> basicClassDefinitionString [
	"Pay attention this definition is fragile in presence of complex slots. Use the public API i.e., classDefinitionString"

	| stream |
	stream := (String new: 800) writeStream.
	forClass superclass
		ifNotNil: [ stream nextPutAll: forClass superclass name ]
		ifNil: [ stream nextPutAll: 'ProtoObject' ].
	stream
		nextPutAll: forClass kindOfSubclass;
		store: forClass name.
	self applyCustomizersOn: stream.
	self instanceVariablesOn: stream.
	self classVariablesOn: stream.
	self poolOn: stream.
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

{ #category : 'internal' }
OldPharoClassDefinitionPrinter >> basicMetaclassDefinitionString [

	"Pay attention this definition is fragile in presence of complex slots. Use the public API i.e., classDefinitionString"

	^ String streamContents: [ :stream |
		  stream print: forClass.
		  self instanceVariablesOn: stream ]
]

{ #category : 'public API' }
OldPharoClassDefinitionPrinter >> classDefinitionString [

	^ forClass needsSlotClassDefinition
		  ifTrue: [ (ClassDefinitionPrinter fluid for: forClass) classDefinitionString ]
		  ifFalse: [ self basicClassDefinitionString ]
]

{ #category : 'low-level elements' }
OldPharoClassDefinitionPrinter >> classVariablesOn: stream [

	stream
		crtab;
		nextPutAll: 'classVariableNames: '''.
	forClass classVariablesOn: stream.
	stream nextPut: $'
]

{ #category : 'low-level elements' }
OldPharoClassDefinitionPrinter >> instanceVariablesOn: stream [

	stream
		crtab;
		nextPutAll: 'instanceVariableNames: '''.
	forClass instanceVariablesOn: stream.
	stream nextPut: $'
]

{ #category : 'public API' }
OldPharoClassDefinitionPrinter >> metaclassDefinitionString [

	^ forClass needsSlotClassDefinition
		ifTrue: [ (ClassDefinitionPrinter fluid for: forClass) metaclassDefinitionString ]
		ifFalse: [ self basicMetaclassDefinitionString ]
]

{ #category : 'low-level elements' }
OldPharoClassDefinitionPrinter >> packageOn: stream [

	stream
		crtab;
		nextPutAll: 'package: ';
		store: forClass category asString
]

{ #category : 'low-level elements' }
OldPharoClassDefinitionPrinter >> poolOn: stream [

	| poolString |
	poolString := forClass sharedPoolsString.
	poolString = '' ifFalse: [
		stream
			crtab;
			nextPutAll: 'poolDictionaries: ';
			store: poolString ]
]
