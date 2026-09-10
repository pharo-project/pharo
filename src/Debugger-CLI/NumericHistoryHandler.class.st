"
""Handles command !<anInteger> and return (if it exists) the command related to the number
	ex : !-2 returns the second last command
		  !2 returns the second command ""
"
Class {
	#name : 'NumericHistoryHandler',
	#superclass : 'DebuggerHistoryHandler',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}
