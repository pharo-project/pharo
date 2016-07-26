actions
addProjectLoadDirective
	"Add a load directive to the selected project."
	
	projectList selectedItem
		ifNil: [ UIManager default inform: 'No project selected' ]
		ifNotNil: [ :requiredProject |
			^ UIManager default request: 'Package or group to load?' 
			"|  potentialRequirements |
			potentialRequirements := project dependencies collect: [ :dependency | dependency name ].
			^ UIManager default chooseFrom: potentialRequirements values: potentialRequirements" ].
	^ nil