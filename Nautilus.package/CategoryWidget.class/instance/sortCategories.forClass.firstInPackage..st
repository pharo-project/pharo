private
sortCategories: allProtocols forClass: class firstInPackage: currentPackage
	"List the protocols in this order: 
		1. the -- all -- category
		2. the extension protocols of the given package
		3. the extension protocols
		4. the normal protocols"
		
	|  sortBlock currentPackageProtocols otherProtocols |

	currentPackageProtocols := allProtocols reject: [ :protocolName | protocolName beginsWith: '*' ].
	
	"Filter out the existing protocols so that we can sort them later separately"
	otherProtocols := allProtocols difference: currentPackageProtocols.
		
	sortBlock := [ :a :b | (a compare: b caseSensitive: false) = 1 ].
	
	^ (currentPackageProtocols sort: sortBlock), (otherProtocols sort: sortBlock)