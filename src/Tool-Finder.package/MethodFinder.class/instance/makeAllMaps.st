argument permutation maps
makeAllMaps 
	"Make a giant list of all permutations of the args.  To find the function, we will try these permutations of the input data.  receiver, args."

	| ii |
	mapList := Array new: argMap size factorial.
	ii := 1.
	argMap permutationsDo: [:perm |
		mapList at: ii put: perm copy.
		ii := ii + 1].
	mapStage := 1.	"about to be bumped"