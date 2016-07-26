*Versionner-Commit-UI
options
	^ {  
	{ 'Nest projects?'. 				#shouldNestProjects}.
	{ 'Commit projects?'. 			#shouldCommitProjects}.
	{ 'Commit all projects (even those not dirty)?'. 	#shouldCommitNestedProjects }.
	{ 'Commit dirty packages?'. 	#shouldCommitDirtyPackages} 
	}