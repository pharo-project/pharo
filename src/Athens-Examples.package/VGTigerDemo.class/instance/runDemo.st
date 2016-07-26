run
runDemo
	| paths extent rotation scale translation rotationSpeed time frames zoom |
	
	paths := self convertPathData2.

	rotationSpeed := 1.
	rotation := 0.
	extent := surface extent.
	scale := extent x / self class tigerMaxY.
	translation := 0@0.

	time := Time millisecondClockValue.
	frames := 0.
	[ Sensor anyButtonPressed ] whileFalse: [
		
		rotation := rotation + rotationSpeed.
		frames := frames + 1.

		zoom := (frames/100) sin abs .
		
		surface 

			drawDuring: [:can |

				can pathTransform 
					loadIdentity. 
					
				can 
					setPaint: Color white;
					drawShape: (0@0 corner: surface extent).
					
				can pathTransform 
					translateX: (0.5 * (extent x - (self class tigerMaxX * scale*zoom))) Y: extent y / -12.0 ;
					scaleBy: scale*zoom ;

					translateX: (self class tigerMaxX * 0.5) Y: (self class tigerMaxY * 0.5 );
					scaleBy: 0.8;
					rotateByDegrees: rotation;
					translateX: (self class tigerMaxX * -0.5) Y: (self class tigerMaxY * -0.5 ).

				paths do: [:each | each renderOn: can ].

			].
			"display it"
			Display getCanvas drawImage: surface asForm  at: 0@0 
	].

	World invalidRect: (World bounds).
	time := Time millisecondClockValue - time.
	
	Transcript show: 'Total rendering time: ', time asString,  ' ms, total frames: ' , frames asString ; cr;
		show: ' FPS: ' , (1000*frames/time) asFloat asString