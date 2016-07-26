accessing
hasBeenModified
	"Return true if something *might* have been drawn into the receiver"
	^(bits == nil or:[bits class == ByteArray]) not
	"Read the above as: If the receiver has forgotten its contents (bits == nil) 
	or is still hibernated it can't be modified."