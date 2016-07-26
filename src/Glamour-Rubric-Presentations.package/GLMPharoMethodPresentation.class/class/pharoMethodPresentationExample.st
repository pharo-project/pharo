examples
pharoMethodPresentationExample
	"
	self pharoMethodPresentationExample
	"
	
	| browser |
	browser := GLMTabulator new.
	browser
		row: #r1;
		row: #r2.
	browser transmit
		to: #r1;
		andShow: [ :aComposite | aComposite pharoMethod display: [ :method | method sourceCode ] ].
	browser transmit
		to: #r2;
		andShow: [ :aComposite | 
					aComposite pharoMethod
						display: [ :method | method sourceCode ];
						smalltalkClass: [ :method | method methodClass ] ].
	browser openOn: CompiledMethod class >> #gtExampleSimple