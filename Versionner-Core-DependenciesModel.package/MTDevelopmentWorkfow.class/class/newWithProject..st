initialization
newWithProject: project
	"Create a new instance dedicated to the project given as parameter."

	^ self new
		project: project;
		yourself