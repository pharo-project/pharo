private
angleOfVector: v

	| n acos |
	
	n := v normalized.
	acos := n x arcCos.
	
	^ v y < 0 ifTrue: [ Float pi * 2 - acos ] ifFalse: [ acos ]