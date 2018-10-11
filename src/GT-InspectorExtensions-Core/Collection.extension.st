Extension { #name : #Collection }

{ #category : #'*GT-InspectorExtensions-Core' }
Collection >> gtCollectionSizeThreshold [
	"Return the maximum collection size that GT supports while showing all elements.
	When this threshold is reached, we no longer show certain views that would take too much resources."

	^ 16rFFFF "65535"
]

{ #category : #'*GT-InspectorExtensions-Core' }
Collection >> gtDisplayOn: stream [
	self printNameOn: stream.
	stream
		space;
		nextPut: $[;
		print: self size;
		nextPutAll: (' item' asPluralBasedOn: self size);
		nextPut: $];
		space.
	self size <= self gtCollectionSizeThreshold 
		ifTrue: [ self printElementsOn: stream ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
Collection >> gtInspectorItemsIn: composite [
	<gtInspectorPresentationOrder: 0> 
	^ (composite fastList)
		title: 'Items';
		display: [ 
			"The FastTable renderer needs to access elements by index so we transform
			the collection to an OrderedCollection. Subclasses can override the #display
			block to provide a different behaviour. The previous renderer had this behaviour
			implemented in the renderer."
			self asOrderedCollection ];
		beMultiple;	
		format: [ :each | GTObjectPrinter asTruncatedTextFrom: each ];
		send: [ :result | result isNil 
			ifTrue:[nil]
			ifFalse:[ (result size = 1) 
				ifTrue: [result anyOne] 
				ifFalse: [self species withAll: result]]]
		"withSmalltalkSearch;	
		showOnly: 50;
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'."
]
