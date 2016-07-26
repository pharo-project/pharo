factory
centerOfAllFingers
	| fingersPosition sum cpt |
	cpt := 0.
	sum := 0@0.
	
	fingersPosition := self activeFingers
		collect: [ :fingerTracker | fingerTracker lastPosition ].
		
	fingersPosition ifEmpty: [ ^ nil ].
	
	fingersPosition do: [ :point | sum := sum + point. cpt := cpt + 1 ].
	
	^ sum / cpt