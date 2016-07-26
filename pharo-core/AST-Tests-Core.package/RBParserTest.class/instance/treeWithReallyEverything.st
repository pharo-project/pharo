private
treeWithReallyEverything
	^ RBParser parseMethod: 'method: arg1 <foo> <foo: 1.2> | temps | temps := #[ 1 2 3 ]. temps := #(true false nil 1 1.2 $a foo #foo ''foo'' #() #(1 2) #[] #[1 2]). { 1 negated. 1 + 2. 1 raisedTo: 2 }. temps foo; foo: self. ^ (temps collect: [:e | | btemps | ((e isNil)) ])'