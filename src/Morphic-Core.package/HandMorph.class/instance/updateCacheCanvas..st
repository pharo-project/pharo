drawing
updateCacheCanvas: aCanvas 
	"Update the cached image of the morphs being held by this hand."

	"Note: The following is an attempt to quickly get out if there's no change"

	| subBnds rectList nPix |
	subBnds := Rectangle merging: (submorphs collect: [:m | m fullBounds]).
	rectList := damageRecorder invalidRectsFullBounds: subBnds.
	damageRecorder reset.
	(rectList isEmpty 
		and: [cacheCanvas notNil and: [cacheCanvas extent = subBnds extent]]) 
			ifTrue: [^self].

	"Always check for real translucency -- can't be cached in a form"
	self submorphsDo: 
			[:m | 
			m wantsToBeCachedByHand 
				ifFalse: 
					[cacheCanvas := nil.
					cachedCanvasHasHoles := true.
					^self]].
	(cacheCanvas isNil or: [cacheCanvas extent ~= subBnds extent]) 
		ifTrue: 
			[cacheCanvas := (aCanvas allocateForm: subBnds extent) getCanvas.
			cacheCanvas translateBy: subBnds origin negated
				during: [:tempCanvas | self drawSubmorphsOn: tempCanvas].
			self submorphsDo: 
					[:m | 
					(m areasRemainingToFill: subBnds) isEmpty 
						ifTrue: [^cachedCanvasHasHoles := false]].
			nPix := cacheCanvas form tallyPixelValues first.
			"--> begin rounded corners hack <---"
			cachedCanvasHasHoles := (nPix = 48 
						and: [submorphs size = 1 and: [submorphs first wantsRoundedCorners]]) 
							ifTrue: [false]
							ifFalse: [nPix > 0].
			"--> end rounded corners hack <---"
			^self].

	"incrementally update the cache canvas"
	cacheCanvas translateBy: subBnds origin negated
		during: 
			[:cc | 
			rectList do: 
					[:r | 
					cc clipBy: r
						during: 
							[:c | 
							c fillColor: Color transparent.
							self drawSubmorphsOn: c]]]