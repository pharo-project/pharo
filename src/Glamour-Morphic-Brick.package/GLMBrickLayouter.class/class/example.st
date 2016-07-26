examples
example

	|bricks|
	
	bricks := OrderedCollection new.
	1 to: 13 do: [ :i | bricks add: GLMBrick new ].
	
	(bricks at: 1) 
		width: 100;
		height: 100.
		
	(bricks at: 2)
		hResizing: #spaceFill;
		height: 20.
		
	(bricks at: 3)
		hResizing: #spaceFill;
		vResizing: #opposite.
		
	(bricks at: 4)
		hResizing: #spaceFill;
		vResizing: #spaceFill.
		
	(bricks at: 1)
		addBrickBack: (bricks at: 2);
		addBrickBack: (bricks at: 3);
		addBrickBack: (bricks at: 4).
		
	(bricks at: 5)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
	
	(bricks at: 6)
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	
	(bricks at: 7)
		width: 50;
		vResizing: #opposite.
		
	(bricks at: 8)
		hResizing: #spaceFill;
		vResizing: #shrinkWrap.
		
	(bricks at: 9)
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	
	(bricks at: 10)
		hResizing: #spaceFill;
		vResizing: #spaceFill.
		
	(bricks at: 11)
		hResizing: #spaceFill;
		height: 20.
		
	(bricks at: 12)
		hResizing: #spaceFill;
		height: 20.
		
	(bricks at: 13)
		hResizing: #spaceFill;
		height: 20.
		
	(bricks at: 2)
		addBrickBack: (bricks at: 5);
		addBrickBack: (bricks at: 6).
		
	(bricks at: 3)
		addBrickBack: (bricks at: 7);
		addBrickBack: (bricks at: 8).
	
	(bricks at: 4)
		addBrickBack: (bricks at: 9);
		addBrickBack: (bricks at: 10).
		
	(bricks at: 5)
		addBrickBack: (bricks at: 11).
		
	(bricks at: 8)
		addBrickBack: (bricks at: 12);
		addBrickBack: (bricks at: 13).
	"self halt."
	^ bricks
	
	