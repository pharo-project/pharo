updating
updatePreviewWithLoading

	self compositeBrick: (
		GLMLabelBrick new
			text: 'Loading...';
			textPosition: #center;
			textColor: Color lightGray;
				with: [ :b | b label
				customFont;
				fontSize: 14 ])