Class {
	#name : #GLMMorphicSpotterTest,
	#superclass : #TestCase,
	#category : 'Glamour-Morphic-Renderer-Spotter'
}

{ #category : #'as yet unclassified' }
GLMMorphicSpotterTest >> creation [
	| composite classRequest methodRequest thirdRequest |
	composite  := GLMSpotterRequest new.

	classRequest := GLMSingleSpotterRequest new 
			prompt: 'Find Class';
			searchBlock: [ :string | Smalltalk allClassesAndTraits select: [:each | string, '*' match: each name] ];
			labelBlock: [ :class | class name ]";
			iconBlock: [ :class | class browserIcon ]".

	methodRequest := GLMSingleSpotterRequest new 
			prompt: 'Object Selectors';
			searchBlock: [ :string | Object selectors select: [:e| string, '*' match: e asString] ];
			labelBlock: [ :e | e ].

	thirdRequest := GLMSingleSpotterRequest new 
			prompt: 'Other Selectors';
			searchBlock: [ :string | Class selectors select: [:e| string, '*' match: e asString] ];
			labelBlock: [ :e | e ].
			
	(composite 
		add: classRequest;
		add: methodRequest;
		add: thirdRequest;
				signal) inspect.
]
