instance creation
newHintBrick

	^ GLMLabelBrick new
		text: self hintText;
		padding: #(0 10);
		width: [ :b | b approximateWidth ];
		height: [ :b | b approximateHeight ]