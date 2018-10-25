Extension { #name : #Color }

{ #category : #'*GT-InspectorExtensions-Core' }
Color >> gtInspectorColorIn: composite [
	<gtInspectorPresentationOrder: 30>
	composite morph 
		title: 'Color';
		display: [ BorderedMorph new color: self ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
Color >> gtInspectorDetailsIn: composite [
	<gtInspectorPresentationOrder: 32>
	^ composite table
		title: 'Details';
		display: [ 
					{'name' -> self name.
					'RGB red' -> self red.
					'RGB green' -> self green.
					'RGB blue' -> self blue.
					'alpha' -> self alpha.
					'HSL hue' -> self hue.
					'HSL saturation' -> self hslSaturation.
					'HSL lightness' -> self lightness.
					'HSV hue' -> self hue.
					'HSV saturation' -> self hsvSaturation.
					'HSV value' -> self brightness.
					'hex' -> self asHexString } ];
		column: 'Key' evaluated: #key;
		column: 'Value' evaluated: #value;
		send: #value
]
