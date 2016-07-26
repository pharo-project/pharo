truncation and round off
rounded
	"Answer the integer nearest the receiver."

	^(self + (self sign / 2)) truncated