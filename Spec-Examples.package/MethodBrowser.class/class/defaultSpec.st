specs
defaultSpec
	<spec>
	| delta |
	delta := 15.
	^ SpecLayout composed 
			add: #listModel origin: 0@0 corner: 1@0.5 offsetOrigin: 0@0 offsetCorner: 0@(delta negated);
			add: #toolbarModel origin: 0@0.5 corner: 1@0.5 offsetOrigin: 0@(delta negated) offsetCorner: 0@delta;
			add: #textModel origin: 0@0.5 corner: 1@1 offsetOrigin: 0@delta offsetCorner: 0@0;
			yourself