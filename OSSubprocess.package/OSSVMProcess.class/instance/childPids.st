childs management
childPids

	^ childProcessList collect: [ :process | process pid ]
