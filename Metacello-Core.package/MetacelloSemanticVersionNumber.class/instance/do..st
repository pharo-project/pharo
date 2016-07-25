enumerating
do: aBlock 
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]