accessing
actionColumn: column
	"If columns has actions associated, execute them"
	column execute.
	self tableRefresh