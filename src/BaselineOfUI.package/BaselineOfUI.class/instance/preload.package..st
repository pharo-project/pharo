actions
preload: loader package: packageSpec

	'Starting preload action' logCr.
	
	Author fullName: self class name.

	Trait named: #TViewModel
	uses: {}
	category: 'Rubric-Editing-Core-Support'.

	Pragma compile: 'methodSelector
	^method selector'.


	
