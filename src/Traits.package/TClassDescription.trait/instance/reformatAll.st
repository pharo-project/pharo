compiling
reformatAll
	"Reformat all methods in this class"
	self methods do: [:method | method reformat]