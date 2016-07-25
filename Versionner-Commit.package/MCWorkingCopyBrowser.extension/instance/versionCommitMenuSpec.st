*Versionner-Commit-UI
versionCommitMenuSpec
	<contextMenu: #configuration>
	
	^ #(#('Commit new PATCH version' #commitNewPatch) 
		#('Commit new MINOR version' #commitNewMinor) 
		#('Commit new MAJOR version' #commitNewMajor)
		#('Open with Versionner' #openWithVersionner))