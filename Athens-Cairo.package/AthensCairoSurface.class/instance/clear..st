accessing
clear: clearPaint
	currentCanvas pathTransform restoreAfter: [
		currentCanvas pathTransform loadIdentity.
		currentCanvas paintMode restoreAfter: [
			currentCanvas paintMode source.
			currentCanvas 
				setPaint: clearPaint;
				drawShape: (0@0 extent: self extent).
				
        ]].

