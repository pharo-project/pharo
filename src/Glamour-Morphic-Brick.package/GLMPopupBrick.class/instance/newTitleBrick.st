instance creation
newTitleBrick

	^ GLMLabelBrick new
		text: self titleText;
		padding: #(0 10);
		width: [ :b | b approximateWidth ];
		height: [ :b | b approximateHeight ]