clipping
clipBy: aRectangle during: aBlock
	| oldClip newClip |

	oldClip := currentClipRect.
	newClip := oldClip ifNil: [ aRectangle ] ifNotNil: [ oldClip intersect: aRectangle ifNone:[^ self] ]. 
	
	self setPathMatrix; resetClip; setClipRect: newClip.
	
	currentClipRect := newClip.

	[aBlock value] ensure: [ 
		self 
		setPathMatrix;
		resetClip;
		setClipRect: oldClip. 
		currentClipRect := oldClip. ].
	
