*GT-InspectorExtensions-Core
gtExampleSimple
	<gtExample>
	^ GLMTabulator new 
		with: [ :t |
			t column: #one.
			t transmit to: #one; andShow: [ :a | a text] ];
		startOn: 42