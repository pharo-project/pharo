async
complete: anObject callback: aBlock
	
	process ifNil: [ ^ self terminate ].
	process isTerminated ifFalse: [ "for some cases when we try to return result of wrong process" ^ self ].
	
	callback = aBlock ifFalse: [ ^ self terminate ].
	
	callback ifNotNil: [ callback cull: anObject ].
	process := nil