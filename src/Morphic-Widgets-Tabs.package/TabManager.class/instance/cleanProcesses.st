private
cleanProcesses
	
	processes do: [ :each | each terminate ].
	processes removeAll.