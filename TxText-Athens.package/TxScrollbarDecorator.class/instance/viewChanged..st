accessing
viewChanged: aTxLayoutViewChanged
	| view |
	
	view := aTxLayoutViewChanged view.
	
	vertical ifFalse: [ 
		| totalSize |
		
		autohide ifTrue: [ 
			"automatically hide if we can"
			view needsHorizontalScrolling ifFalse: [ self hide. ^ self ]].
		
		self visible: true.
		
		totalSize := view layout extent x.
		
		totalSize isZero 
			ifTrue: [ scrollbar sliderPosition: 0; thickness: 1 ]
			ifFalse: [ 
				scrollbar 
					sliderPosition: view offset x / totalSize;
					thickness: (view width/ totalSize min: 1.0)].
	] ifTrue: [ 
		"vertical one"
		| numberOfLines |

		autohide ifTrue: [ 
			"automatically hide if we can"
			view needsVerticalScrolling ifFalse: [ self hide. ^ self ]].

		self visible: true.
		numberOfLines := view text endPosition lineNumber.
		
		scrollbar thickness: (view layout numberOfLines / numberOfLines).
		scrollbar sliderPosition: (view layout startPosition lineNumber - 1) / numberOfLines. 
	
	 ]