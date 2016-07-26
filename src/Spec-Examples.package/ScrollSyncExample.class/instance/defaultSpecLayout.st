layouts
defaultSpecLayout
	<spec>
	 ^ SpecLayout composed
		add: #sideRuler 
		origin: 0@(1 - self currentVerticalScrollPosition) 
		corner: 0@(1 - self currentVerticalScrollPosition) 
		offsetOrigin: 0@(-250) 
		offsetCorner: 50@250;
		add: #text 
		origin: 0@0 
		corner: 1@1 
		offsetOrigin: 50@0 
		offsetCorner: 0@0;
		yourself