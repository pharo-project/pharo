Extension { #name : #PasteUpMorph }

{ #category : #'*Tool-Base' }
PasteUpMorph >> defaultDesktopCommandKeyTriplets [
	"Answer a list of triplets of the form
		<key> <receiver> <selector>   [+ optional fourth element, a <description> for use in desktop-command-key-help]
that will provide the default desktop command key handlers.  If the selector takes an argument, that argument will be the command-key event"
	self flag: #problem. 
	"there is clearly a problem because not all the tools can be loaded."
 
	^ {
		{ $r.	self currentWorld.						#restoreMorphicDisplay.					'Redraw the screen'}.		
		{ $b.	Smalltalk tools browser.			#open.										'Open a new System Browser'}.
		{ $k.	Smalltalk tools workspace.			#open.										'Open a new, blank Workspace'}.
		{ $t.	Smalltalk tools transcript.			#open.										'Make a System Transcript visible'}.
		{ $W.	Smalltalk tools finder. 				#open.						         		  	'Open a new Finder'}.
		{ $Z.	Smalltalk tools changeList. 			#browseRecentLog.						'Browse recently-logged changes'}.
		{ $\.	SystemWindow. 					#sendTopWindowToBack.					'Send the top window to the back'}.
	}.

]
