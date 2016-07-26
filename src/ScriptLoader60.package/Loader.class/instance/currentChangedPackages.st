compute
currentChangedPackages
	"self new currentChangedPackages" 
	
	^  self currentPackages select: [:each |
		each needsSaving or: [
			(PackagesBeforeLastLoad includes: each ancestry ancestorString) not ] ]