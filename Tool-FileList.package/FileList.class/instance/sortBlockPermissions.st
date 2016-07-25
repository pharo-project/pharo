private
sortBlockPermissions
	"Answer block to decide what order to display the directory entries."

	^ [ :x :y | |xIsDir|
			((xIsDir := x isDirectory) = y isDirectory) 
				ifTrue: [   x permissions <= y permissions  ]
				ifFalse: [
					"directories always precede files"
					xIsDir ]]