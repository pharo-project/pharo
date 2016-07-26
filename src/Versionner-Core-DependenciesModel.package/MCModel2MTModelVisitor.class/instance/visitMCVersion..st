visiting
visitMCVersion: aVersion
	"visit a Metacello Version"
	
	| groups packages projects repositories |
	
	project version
		description: aVersion spec description value;
		author: aVersion spec author value;
		timestamp: aVersion spec timestamp value.

	groups := aVersion groups.
	groups do:  [ :aGroup | self visitMCGroup: aGroup ].
	
	packages := aVersion packages.
	packages do:  [ :aPackage | self visitMCPackage: aPackage ].
	
	projects := aVersion projects.
	projects do:  [ :aProject | self visitMCProjectAsRequiredProject: aProject ].
	
	repositories := aVersion spec repositories list.
	repositories ifNotEmpty:  
		[:repo | project repository: repo first name].
