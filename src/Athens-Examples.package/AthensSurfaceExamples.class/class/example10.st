examples
example10 
" draw a rounded rectangle, using the arc segments

AthensCairoSurfaceExamples example10.
"

	| surf |
	
	surf := self newSurface: 440@440.
	
	surf drawDuring: [:can |
		| p |
		surf clear.
		can pathTransform
			scaleBy: 3;
			translateX: 10 Y: 10.
		p:= can createPath: [:path | |halfPi |
				halfPi := Float pi /2.
				path 
					relative;
					moveTo: 10@0;
					lineTo: 40@0;
					ccwArcTo: 10@10 angle: halfPi;
					lineTo: 0@40;
					cwArcTo: -10@10 angle: halfPi;
					lineTo: -40@0;
					cwArcTo: -10@ -10 angle: halfPi;
					lineTo: 0@ -40;
					ccwArcTo: 10@ -10 angle: halfPi].
				
		can setShape: p.
		can setPaint: Color white.
		can draw.
		can setStrokePaint: Color red.
		can draw ].
		
	Display getCanvas drawImage: 	surf asForm  at: 0@0
