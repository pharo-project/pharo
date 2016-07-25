correcting
openMenuIn: aBlock
	| alternatives labels actions lines caption choice name interval |
	interval := node sourceInterval.
	name := node name.
	alternatives := self possibleVariablesFor: name.
	labels := OrderedCollection new.
	actions := OrderedCollection new.
	lines := OrderedCollection new.
	name first isLowercase
		ifTrue: [ 
			labels add: 'Declare new temporary variable'.
			actions add: [ self declareTempAndPaste: name ].
			labels add: 'Declare new instance variable'.
			actions add: [ self declareInstVar: name ] ]
		ifFalse: [ 
			labels add: 'Define new class'.
			actions
				add: [ 
					[ self defineClass: name ]
						on: Abort
						do: [ self openMenuIn: aBlock ] ].
			labels add: 'Declare new global'.
			actions add: [ self declareGlobal ].
			labels add: 'Declare new class variable'.
			actions add: [ self declareClassVar ] ].
	lines add: labels size.
	alternatives
		do: [ :each | 
			labels add: each.
			actions
				add: [ 
					self substituteVariable: each atInterval: interval.
					(OCSourceCodeChanged new newSourceCode: self requestor text) signal ] ].
	lines add: labels size.
	labels add: 'Cancel'.
	caption := 'Unknown variable: ' , name , ' please correct, or cancel:'.
	choice := aBlock value: labels value: lines value: caption.
	choice ifNotNil: [ self resume: (actions at: choice ifAbsent: [ compilationContext failBlock value ]) value ]