baseline
project
	
	| aProject |
	
	aProject := super project.
	aProject loadType: #atomic.
	^ aProject.
	