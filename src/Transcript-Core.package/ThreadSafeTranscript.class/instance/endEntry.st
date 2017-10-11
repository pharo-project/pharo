streaming
endEntry
"	Signal to #step to display all the characters since the last #step. 
	Stream is reset there.
"  
  	self critical: [ deferredEndEntry := true ].