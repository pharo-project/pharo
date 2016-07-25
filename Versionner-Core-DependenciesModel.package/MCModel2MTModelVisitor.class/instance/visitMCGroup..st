visiting
visitMCGroup: aGroup
	"visit a Metacello Group"	
	| group |

	group := MTGroup new.
	group name: aGroup name.
	aGroup includes do: 
		[ :dep | 
		group addDependency: dep].
	project addDependency: group.
	^ group.
	