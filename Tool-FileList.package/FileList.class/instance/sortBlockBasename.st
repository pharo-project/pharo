private
sortBlockBasename
	"Answer block to decide what order to display the directory entries."

	^ [ :x :y | |xIsDir|
			((xIsDir := x isDirectory) = y isDirectory) 
				ifTrue: [   x basename <= y basename  ]
				ifFalse: [
					"directories always precede files"
					xIsDir ]]