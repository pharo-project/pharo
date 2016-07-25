private
determineActivePlatform
	"Look for the matching platform class"
	^ self allSubclasses 
		detect: [ :any | any isActivePlatform ].
