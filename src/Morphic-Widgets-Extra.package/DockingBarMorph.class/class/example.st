example
example
	"self example"
	<exampleWidget>
	
	DockingBarMorph new 
		addMorph: (SimpleButtonMorph new
                                           label: 'Say hello';
                                           target: [UIManager inform: 'Hello'];
                                           actionSelector: #value);
		addMorph: (SimpleButtonMorph new
                                           label: 'Say bonjour';
                                           target: [UIManager inform: 'Bonjour'];
                                           actionSelector: #value);
		addMorph: (SimpleButtonMorph new
                                           label: 'Close';
                                           target: [DockingBarMorph allInstances last delete];
                                           actionSelector: #value);
	adhereToBottom;
	openInWorld.