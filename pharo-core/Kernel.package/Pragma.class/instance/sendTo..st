processing
sendTo: anObject
	"Send the pragma keyword together with its arguments to anObject and answer the result."
	
	^ anObject perform: self keyword withArguments: self arguments