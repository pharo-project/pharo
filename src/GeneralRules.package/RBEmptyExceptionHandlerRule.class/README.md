Empty exception handler blocks hide potential bugs. The situation should be handled in a more robust way.

[  .... ]
	on: Error
	do: [  ]
	
having an empty block is a bad idea because the program silently fails.