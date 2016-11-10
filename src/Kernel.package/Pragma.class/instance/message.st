accessing-pragma
message
	"Answer the message of the receiving pragma."
	
	^ Message selector: self selector arguments: self arguments. 