private
removeAllHandlesBut: aHandle
	"Remove all handles except aHandle."
	submorphs copy do: [:m | m == aHandle ifFalse: [m delete]]
