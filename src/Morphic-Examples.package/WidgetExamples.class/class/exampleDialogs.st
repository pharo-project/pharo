examples
exampleDialogs
	"Open an example of each services dialog"
	"self exampleDialogs"
	<script>
	
	self exampleBuilder
		chooseFont: TextStyle default defaultFont;
		chooseColor: (Color r: 0.529 g: 0.611 b: 0.004);
		chooseDirectory: 'Choose folder';
		chooseFileName: 'Pick a file name' extensions: nil path: nil preview: nil;
		textEntry: 'Choose a name for the project' title: 'Save project' entryText: 'My Project';
		chooseDropList: 'Pick one!' list: #('One' 'Two' 'Three' 'Four');
		deny: 'Opening the safe is not possible!';
		message: 'Hello!';
		proceed: 'Save image?';
		alert: 'Danger!';
		abort: 'Something has gone wrong...';
		question: 'Save the file?' title: 'Exiting application';
		questionWithoutCancel: 'Save image as well?' title: 'Exiting application'