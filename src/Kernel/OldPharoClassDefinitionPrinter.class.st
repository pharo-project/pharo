Class {
	#name : #OldPharoClassDefinitionPrinter,
	#superclass : #ClassDefinitionPrinter,
	#category : #'Kernel-ClassDefinitionPrinter'
}

{ #category : #delegating }
OldPharoClassDefinitionPrinter >> classDefinitionString [

	| poolString stream |
	poolString := forClass sharedPoolsString.
	stream := (String new: 800) writeStream.
	forClass superclass
		ifNotNil: [ stream nextPutAll: forClass superclass name ]
		ifNil: [ stream nextPutAll: 'ProtoObject' ].
	
	stream
		nextPutAll: forClass kindOfSubclass;
		store: forClass name.
		
	forClass hasTraitComposition ifTrue: [ 
		stream
			crtab;
			nextPutAll: 'uses: ';
			nextPutAll: forClass traitCompositionString ].
	
	stream
		crtab;
		nextPutAll: 'instanceVariableNames: '''.
	forClass instanceVariablesOn: stream.
	stream nextPut: $'.
	
	stream
		crtab;
		nextPutAll: 'classVariableNames: '''.
	forClass classVariablesOn: stream.
	stream nextPut: $'.
	
	poolString = '' ifFalse: [ 
		stream 
			crtab;
			nextPutAll: 'poolDictionaries: ';
			store: poolString ].
	
	stream
		crtab;
		nextPutAll: 'package: ';
		store: forClass category asString.
		
	forClass superclass ifNil: [ 
		stream
			nextPutAll: '.';
			cr.
		stream 
			nextPutAll: forClass name.
		stream
			space;
			nextPutAll: 'superclass: nil' ].

	^ stream contents
]

{ #category : #template }
OldPharoClassDefinitionPrinter >> classDefinitionTemplateInPackage: aString [

	^ String streamContents: [ :s |
			s nextPutAll: 'Object subclass: #MyClass' ; crtab.
			s nextPutAll: 'instanceVariableNames: '''' '; crtab. 
			s nextPutAll: 'classVariableNames: '''''; crtab.
			s nextPutAll: 'package: ''', aString, '''' ]
]

{ #category : #delegating }
OldPharoClassDefinitionPrinter >> expandedDefinitionString [
	"We do not support expanded on old Pharo syntax so we shortcut the double dispatch call."
	^ self definitionString 
]

{ #category : #delegating }
OldPharoClassDefinitionPrinter >> metaclassDefinitionString [ 

	^ String streamContents: [:stream |
		stream print: forClass.
		stream
			crtab;
			nextPutAll: 'instanceVariableNames: '''.
		forClass instanceVariablesOn: stream.
		stream nextPut: $' ]
]

{ #category : #delegating }
OldPharoClassDefinitionPrinter >> traitDefinitionString [ 
	"Next step
		- some of the methods defined on classe will have to be moved in this class.
		- refactor to remove duplication with metaclass,...."

	(forClass instanceSide name == #Trait) ifTrue: [ ^ self classDefinitionString ].

	^ String streamContents: [ :s | 
		s 
			nextPutAll: 'Trait named: ';
			nextPutAll:	forClass name printString;
			cr; tab; 
			nextPutAll: ' uses: ';
			nextPutAll: forClass traitComposition traitCompositionExpression; 
			cr.
		"Important"
		self flag: #todo.
		"this class definition should NOT use slots: 
			either this is a simple slot and we use instance variable names
			or this is a complex slot and we use the fluid definition.
			we could have forClass neslotsNeedFullDefinition
					ifTrue: [ (self class fluid forClass: self) traitDefinitionString ] "
		forClass classLayout visibleSlots ifNotEmpty: [ 
			s tab; nextPutAll: ' slots: ';
			nextPutAll: forClass slotDefinitionString; cr. ].
		"End of important"
		
		s tab; nextPutAll: ' package: ';
			nextPutAll: forClass category asString printString
	]
]

{ #category : #template }
OldPharoClassDefinitionPrinter >> traitDefinitionTemplateInPackage: aString [

	^ String streamContents: [ :s |
			s nextPutAll: 'Trait named: #TMyTraits
	 uses: {}
	 package: '.
			s nextPutAll: aString ]
 
]

{ #category : #delegating }
OldPharoClassDefinitionPrinter >> traitedMetaclassDefinitionString [ 

	^ String streamContents: [:strm |
			strm print: forClass.
			forClass hasTraitComposition ifTrue: [
				strm
					crtab;
					nextPutAll: 'uses: ';
					print: forClass traitComposition ].
				strm
					crtab;
					nextPutAll: 'instanceVariableNames: ';
					store: forClass instanceVariablesString ]
]
