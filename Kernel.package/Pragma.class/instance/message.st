accessing-pragma
message
	"Answer the message of the receiving pragma."
	
	^ Message selector: self keyword arguments: self arguments. 