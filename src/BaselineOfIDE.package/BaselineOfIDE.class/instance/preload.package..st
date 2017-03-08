actions
preload: loader package: packageSpec

	'Starting preload action' logCr.
	
	Author fullName: self class name.

	Object subclass: #ReAbstractRule
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Renraku-Rules'.
	
	(self class environment at: #ReAbstractRule) subclass: #ReNodeBasedRule
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Renraku-Rules'.

	(self class environment at: #ReNodeBasedRule) subclass: #ReNodeRewriteRule
	instanceVariableNames: 'matches'
	classVariableNames: ''
	package: 'Renraku-Rules'.
	
