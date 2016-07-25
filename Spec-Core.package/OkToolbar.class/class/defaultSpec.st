specs
defaultSpec
	<spec>
	
	^ SpecLayout composed
		add: #okButton origin: 1@0 corner: 1@1 offsetOrigin: (self buttonWidth negated)@0 offsetCorner: 0@0;
		send: #vShrinkWrap;
		yourself