change set
removePostscriptFrom: aChange

	(aChange hasPostscript and: [ aChange postscriptHasDependents ]) ifTrue:
		[^ self inform:
'Cannot remove the postscript right
now because there is at least one
window open on that postscript.
Close that window and try again.'].

	aChange removePostscript.