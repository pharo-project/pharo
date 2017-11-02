Extension { #name : #CompiledCode }

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorASTIn: composite [
	<gtInspectorPresentationOrder: 35> 
	(GTSimpleRBTreeBrowser new treeIn: composite)
		title: 'AST';
		display: [ :anObject | {anObject ast} ]
		
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorBytecodeIn: composite [
	<gtInspectorPresentationOrder: 30> 
	^ (GTBytecodeBrowser new treeIn: composite)
		title: 'Bytecode'
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorBytesIn: composite [
	<gtInspectorPresentationOrder: 30>
	"It makes no sense to view me as a collection of bytes"
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorHeaderIn: composite [
	<gtInspectorPresentationOrder: 40> 
	composite text 
		title: 'Header';
		display: [ self headerDescription ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorIrIn: composite [
	<gtInspectorPresentationOrder: 35> 
	composite text 
		title: 'Ir';
		display: [ :anObject || string |
				string := anObject ir longPrintString.
				string copyFrom: 2 to: string size ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorItemsIn: composite [
	"It makes no sense to view me as a collection of items"
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorPragmasIn: composite [
	<gtInspectorPresentationOrder: 30> 
	composite tabulator 
		title: 'Pragmas';
		when: [ :x | x pragmas notEmpty ];
		with: [ :t |  
			t row: #pragmas; row: #methods.
			t transmit to: #pragmas; andShow: [:a | 
				a list
					title: 'Pragmas';
					display: [ :method | method pragmas ];
					when: [ :x | x pragmas notEmpty ] ].
			t transmit from: #pragmas; to: #methods; andShow: [:a | 
				a list 
					title: 'Methods';
					display: [ :pragma | 
						| methods | 
						methods := OrderedCollection new.
						Object withAllSubclassesDo: [ :each | 
							methods addAll: ((Pragma allNamed: pragma keyword in: each) collect: #method) ].
						methods ];
					format: #selector ].
			t transmit
				toOutsidePort: #selection;
				from: #methods port: #selection
		];
		startOn: self
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorSourceIn: composite [
	<gtInspectorPresentationOrder: 30>
	^ composite pharoMethod
		title: 'Source';
		smalltalkClass: [ self methodClass ];
		display: [ self sourceCode ];
		act: [ self browse ]
			icon: GLMUIThemeExtraIcons glamorousBrowse
			entitled: 'Browse'
]

{ #category : #'*GT-InspectorExtensions-Core' }
CompiledCode >> gtInspectorVariableNodesIn: aCollection [

	aCollection addAll: (self literals collectWithIndex: [ :aLiteral :anIndex | 
		GTInspectorDynamicNode hostObject: self label: ('literal', anIndex asString) value: aLiteral ]).
	self initialPC to: self size do: [ :index |
		aCollection add: (GTInspectorDynamicNode hostObject: self label: ('bc ', index asString) value: (self at: index) ) ] 
	

]
