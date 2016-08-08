examples
grid22WithPointLocationCreatedWithRows
	"self grid22WithPointLocationCreatedWithRows"
	"> 
	((1@1) (2@1)
	(1@2) (2@2) )
	"
	| grid |
	grid := self withRows: 
		{  { 1@1 . 2@1 } . 
			{ 1@2 . 2@2 }
		   }.
	^ grid 