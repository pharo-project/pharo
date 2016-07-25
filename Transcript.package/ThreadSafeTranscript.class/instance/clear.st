streaming
clear 
"	Clear all characters by resetting the stream and voiding any previously flagged deferredEndEntry.
	Redisplays the view by signalling for #step to send #changed: .
"	
	self 
		critical: [ 
			deferredClear := true.
			deferredEndEntry := false.
			stream reset ]