processing
sendTo: anObject
	"Send the pragma keyword together with its arguments to anObject and answer the result."

	^ anObject perform: self selector withArguments: self arguments