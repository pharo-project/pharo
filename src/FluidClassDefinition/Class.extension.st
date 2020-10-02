Extension { #name : #Class }

{ #category : #'*FluidClassDefinition' }
Class >> asFluidDefinitionString [
	^ String streamContents: [ :s |
		superclass 
			ifNotNil: [s nextPutAll: superclass name]
			ifNil: [s nextPutAll: 'nil'].
		s 
			nextPutAll: ' < #';
			nextPutAll: name;
			cr.
		
		self classLayout isFixedLayout ifFalse: [
			s 
				tab; 
				nextPutAll: 'layout: ';
				nextPutAll: self classLayout class name;
				nextPutAll: ';';
				cr ].
		
		self hasTraitComposition ifTrue: [
			s 
				tab; 
				nextPutAll: 'uses: ';
				nextPutAll: self traitCompositionString ;
				nextPutAll: ';';
				cr ].
		
		self slots ifNotEmpty: [ 
					s tab.
					s nextPutAll: 'slots: '.
					self slotDefinitionStringOn: s. 
					s nextPutAll: ';'.
					s cr ].
		self classVariables ifNotEmpty: [  
				s 
					tab;
					nextPutAll: 'sharedVariables: ';
					nextPutAll: self classVariableDefinitionString;
					nextPutAll: ';';
					cr
				].
			
		self sharedPools ifNotEmpty: [:pools |
			s 
				tab ; 
				nextPutAll: 'sharedPools: { '.
			pools do: 	[ :p | s nextPutAll: p name ] separatedBy: [ s nextPutAll: ' . ' ].
			s	nextPutAll: ' };' ;
				cr ].	
			
		self tagOn: s.
				
		s 	
			tab;
			nextPutAll: 'package: '; 
			nextPut: $';
			nextPutAll: self package packageName;
			nextPut: $'
		]
]

{ #category : #'*FluidClassDefinition' }
Class >> classVariableDefinitionString [
	"Answer a string that evaluates to the definition of the class Variables"
	
	^String streamContents: [ :str | | fullDef |
		self classVariables 
			ifEmpty: [ str nextPutAll: '{}' ]
			ifNotEmpty:[  :clsVars|
		str nextPutAll: '{ '.
		clsVars do: [:global |
				str nextPutAll: global definitionString.
				fullDef := global needsFullDefinition]				
			separatedBy: [ 
				str nextPutAll: ' . '.  
				fullDef ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]]
]

{ #category : #'*FluidClassDefinition' }
Class >> tagOn: str [
	| tag |
	(self package classTagForClass: self) 
			ifNotNil: [:t | 	
					tag := t name.
					tag = self package name
						ifFalse:  [
					str 	tab;  
					 	nextPutAll: 'tag: '; 
						nextPut: $'; 
						nextPutAll: tag asString ; 
						nextPutAll: ''';'.
					str cr
				]].
]
