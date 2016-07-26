private
treeWithEverything
	^ RBParser parseMethod: 'method: arg1 | temps | temps := #(10). temps foo; foo. ^(temps collect: [:e | ])'