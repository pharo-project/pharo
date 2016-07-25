open/close
open: fileName forWrite: writeMode
	"Since this is an attacheable stream it means the underlying stream in the OS
	has already been opened"
	
	self shouldNotImplement
