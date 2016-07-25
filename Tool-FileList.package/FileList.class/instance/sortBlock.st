private
sortBlock
	"Answer block to decide what order to display the directory entries."

	^ sortBlock ifNil: [ sortBlock := self sortBlockBasename ]