updating
updateOrientation: aPoint
	"Set the layout for the new extent."

	|butts|
	butts := self buttons.
	aPoint x >= aPoint y
		ifTrue: [self listDirection: #leftToRight.
				butts first
					roundedCorners: #(1 2);
					hResizing: #rigid;
					vResizing: #spaceFill;
					width: aPoint y;
					label: (self newButtonLabel: #left ofSize: aPoint y // 2).
				butts last
					roundedCorners: #(3 4);
					hResizing: #rigid;
					vResizing: #spaceFill;
					width: aPoint y;
					label: (self newButtonLabel: #right ofSize: aPoint y // 2)]
		ifFalse: [self listDirection: #topToBottom.
				butts first
					roundedCorners: #(1 4);
					hResizing: #spaceFill;
					vResizing: #rigid;
					height: aPoint x;
					label: (self newButtonLabel: #top ofSize: aPoint x // 2).
				butts last
					roundedCorners: #(2 3);
					hResizing: #spaceFill;
					vResizing: #rigid;
					height: aPoint x;
					label: (self newButtonLabel: #bottom ofSize: aPoint x // 2)]