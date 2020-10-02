Extension { #name : #ClassDescription }

{ #category : #'*FluidClassDefinition' }
ClassDescription >> asFluidDefinitionString [
	^ self subclassResponsibility
]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> definition [
	"Answer a String that defines the receiver."

	Slot showSlotClassDefinition ifTrue: [ 
		^ self asFluidDefinitionString ].
	
	^ self definitionWithoutSlots
	
]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> definitionExpanded [
	"This definition presents all the receiver state also the empty one so that the user can just type and empty elements will be ignored by the builder and parser. 
	"
	"For now I will check here for Slot 
	"
	^ Slot showSlotClassDefinition 
		ifTrue: [
				String streamContents: [ :s | 
						| tag |
						s nextPutAll: 'Object < #', self name asSymbol; crtab.
						
						s 
							nextPutAll: 'layout: ';
							nextPutAll: self classLayout class name; 
							nextPutAll: ';';
							crtab. 
						
						self hasTraitComposition 
							ifTrue: [
								s 
									nextPutAll: 'uses: ';
									nextPutAll: self traitCompositionString ]
							ifFalse: [ s nextPutAll: 'uses: {};'].
						s crtab.

						s nextPutAll: 'slots: ', self slotDefinitionString, ';'; crtab. 
						s 	
							nextPutAll: 'sharedVariables: ';
							nextPutAll: self classVariableDefinitionString;
							nextPutAll: ';'; crtab.
							
						s nextPutAll: 'sharedPools: {'.
						self sharedPoolStringOn: s.
						s nextPutAll: '};' ; crtab.
						
						(self package classTagForClass: self) ifNotNil: [:t | 
							tag := t name.
							tag = self package name
								ifFalse:  [
									s nextPutAll: 'tag: '; 
					nextPut: $'; 
					nextPutAll: tag asString ; 
					nextPutAll: ''';'.
					s crtab
				]].
				
				s 
					nextPutAll: 'package: ''';
					nextPutAll: self package packageName;
					nextPutAll: '''' ]]
		ifFalse: [  self definitionWithoutSlots	]

]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> definitionFullyExpanded [
	"This definition propose all the with an empty arguments so that the user can just type and empty elements will be ignored by the builder and parser. Should check that this is true and write tests."
	
	^ self definitionFullyExpandedButPackage: 'MyPackage'
]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> definitionFullyExpandedButPackage: aString [
	"This definition propose all the with an empty arguments so that the user can just type and empty elements will be ignored by the builder and parser. Should check that this is true and write tests. 
	"
	"For now I will check here for Slot 
	"
	^ Slot showSlotClassDefinition 
		ifTrue: [
				String streamContents: [ :s |
						s nextPutAll: 'Object < #MyClass'; crtab.
						s nextPutAll: 'layout: FixedLayout;'; crtab.
						s nextPutAll: 'uses: {};'; crtab.
						s nextPutAll: 'slots: {};'; crtab. 
						s nextPutAll: 'sharedVariables: {};'; crtab.
						s nextPutAll: 'sharedPools: {};'; crtab.
						s nextPutAll: 'tag: '''' ;';crtab.
						s nextPutAll: 'package: ''', aString, '''' ] 
					]
		ifFalse: [  
				String streamContents: [ :s |
						s nextPutAll: 'Object subclass: #MyClass' ; crtab.
						s nextPutAll: 'instanceVariableNames: '''' '; crtab. 
						s nextPutAll: 'classVariableNames: '''''; crtab.
						s nextPutAll: 'package: ''', aString, '''' ] 				
				]

]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> definitionWithSlots [
	"The class definition with a way to specify slots. Shown when the class defines special Slot"
	^ self instanceSide asFluidDefinitionString
	
	"
	| stream poolString|
	poolString := self sharedPoolsString.

	stream := (String new: 800) writeStream.
	superclass 
		ifNotNil: [stream nextPutAll: superclass name]
		ifNil: [stream nextPutAll: 'ProtoObject'].
	stream 
		nextPutAll: ' subclass: '; 
		store: self name.
	self hasTraitComposition ifTrue: [
		stream 
			crtab; 
			nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString ].
			
	self classLayout isFixedLayout ifFalse: [
		stream 
			crtab; 
			nextPutAll: 'layout: ';
			nextPutAll: self classLayout class name ].
	
	stream 
		crtab; 
		nextPutAll: 'slots: ';
		nextPutAll: self slotDefinitionString.
		
	stream 
		crtab; 
		nextPutAll: 'classVariables: ';
		nextPutAll: self classVariableDefinitionString.
	
	poolString = '' ifFalse: [
		stream 
			crtab; 
			nextPutAll: 'poolDictionaries: ';
			store: poolString ].
		
	stream 
		crtab; 
		nextPutAll: 'package: ';
		store: self category asString.

	superclass ifNil: [ 
		stream nextPutAll: '.'; cr.
		stream nextPutAll: self name.
		stream space; nextPutAll: 'superclass: nil'. ].

	^ stream contents"
]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> definitionWithoutSlots [

	| poolString stream |
	poolString := self sharedPoolsString.
	stream := (String new: 800) writeStream.
	superclass
		ifNotNil: [ stream nextPutAll: superclass name ]
		ifNil: [ stream nextPutAll: 'ProtoObject' ].
	
	stream
		nextPutAll: self kindOfSubclass;
		store: self name.
	self hasTraitComposition ifTrue: [ 
		stream
			crtab;
			nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString ].
	stream
		crtab;
		nextPutAll: 'instanceVariableNames: ';
		store: self instanceVariablesString.
	stream
		crtab;
		nextPutAll: 'classVariableNames: ';
		store: self classVariablesString.
	
	poolString = '' ifFalse: [ 
		stream 
			crtab;
			nextPutAll: 'poolDictionaries: ';
			store: poolString ].
	
	stream
		crtab;
		nextPutAll: 'package: ';
		store: self category asString.
		
	superclass ifNil: [ 
		stream
			nextPutAll: '.';
			cr.
		stream 
			nextPutAll: self name.
		stream
			space;
			nextPutAll: 'superclass: nil' ].

	^ stream contents
	
]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> sharedPoolString [
	"Answer a string of my shared pools separated by spaces."

	^String streamContents: [ :stream | 
		self sharedPoolStringOn: stream ]
]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> sharedPoolStringOn: aStream [
	"Answer a string of my shared pools separated by spaces."

		self sharedPools 
			do:  [ :p | aStream space. 
							aStream nextPutAll: p name.
							aStream space ] 
			separatedBy: [ aStream nextPutAll: '.' ]
]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> slotDefinitionString [
	"Answer a string that contains an executable description of my Slots"
	
	^String streamContents: [ :str | self slotDefinitionStringOn: str]
]

{ #category : #'*FluidClassDefinition' }
ClassDescription >> slotDefinitionStringOn: aStream [
	"Write on the arg aStream an executable description of my Slots"

	| useFull |
	aStream nextPutAll: '{'.
	self localSlots
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
