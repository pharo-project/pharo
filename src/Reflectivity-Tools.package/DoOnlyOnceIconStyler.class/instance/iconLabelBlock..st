defaults
iconLabelBlock: aNode

	^ [Smalltalk globals 
			at: #OneShotArmed
			ifPresent: [ :val | val ifTrue: ['active' ] ifFalse: ['disabled, click to activate' ]]
			ifAbsent: [ 'active' ]
		]