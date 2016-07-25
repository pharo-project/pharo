lessons
instantiation
	^ Lesson
title: 'Instantiation' 
lesson: 
'"Objects are instances of their class. Usually, we send the message #new to a class for creating an instance of this class.

The message #allInstances sent to a class answers an Array with all instances of this class.

For example, let''s look at how many instances of SimpleButtonMorph exist:"

SimpleButtonMorph allInstances size.

"Now create a new instance of it:"

SimpleButtonMorph new
	label: ''A nice button'';
	openCenteredInWorld.

"See the button centered on the world ? The list of all instances should contains one more instance:"

SimpleButtonMorph allInstances size.

"Let''s play with it:"

SimpleButtonMorph allInstances last 
	label: ''ProfStef is cooooool !'';
	color: Color cyan.
	
"Let''s delete it and ask the system to clean the memory:"

SimpleButtonMorph allInstances last delete.
Smalltalk garbageCollect.
SimpleButtonMorph allInstances size.

"Click on the button to go to next lesson:"

SimpleButtonMorph new
	label: ''Go to next lesson'';
	target: [ProfStef next. 
			SimpleButtonMorph allInstances last delete];
	actionSelector: #value;
	openCenteredInWorld.'