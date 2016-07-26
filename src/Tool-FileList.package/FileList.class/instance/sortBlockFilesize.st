private
sortBlockFilesize
	"Answer block to decide what order to display the directory entries."

	^ [ :x :y | |xIsDir|
			((xIsDir := x isDirectory) = y isDirectory) 
				ifTrue: [   x size <= y size  ]
				ifFalse: [
					"directories always precede files"
					xIsDir ]]