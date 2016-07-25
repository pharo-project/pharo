examples
gtExampleWithPopup
	<gtExample>
	<label: 'Label with text and popup showing when hovered with mouse'>
	
	^ self new
		enable;
		text: 'Hello';
		textPosition: #center;
		familyName: 'Bitmap DejaVu Sans';
		fontSize: 11;
		padding: 5;
		with: [ :b  | b
			when: #onMouseHover do: [ GLMPopupBrick new beHelp; openFor: b ] ]